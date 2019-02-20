module Service.Mail.Mailer
  ( sendRegistrationConfirmationMail
  , sendRegistrationCreatedAnalyticsMail
  , sendResetPasswordMail
  ) where

import Control.Exception (SomeException, catch)
import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.HashMap.Strict (fromList)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as U
import qualified Network.HaskellNet.Auth as Auth
import qualified Network.HaskellNet.SMTP as SMTP
import qualified Network.HaskellNet.SMTP.SSL as SMTPSSL
import qualified Network.Mail.Mime as MIME
import qualified Network.Mail.SMTP as SMTPMail
import Text.Ginger (parseGingerFile)

import Constant.Component
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.User.User
import Service.Template.TemplateUtils (mLoadFile, render)
import Util.Logger

sendRegistrationConfirmationMail :: Email -> U.UUID -> String -> AppContextM ()
sendRegistrationConfirmationMail email userId hash = do
  dswConfig <- asks _appContextConfig
  htmlTemplate <- liftIO $ parseGingerFile mLoadFile "templates/mail/registrationConfirmation.html.j2"
  plainTemplate <- liftIO $ parseGingerFile mLoadFile "templates/mail/registrationConfirmation.txt.j2"
  let clientAddress = dswConfig ^. clientConfig . address
      activationLink = clientAddress ++ "/signup-confirmation/" ++ U.toString userId ++ "/" ++ hash
      mailName = dswConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": Confirmation Email"
      context = fromList [("activationLink", activationLink), ("mailName", mailName)]
      htmlBody = makeMailPart htmlTemplate SMTPMail.htmlPart context
      plainBody = makeMailPart plainTemplate SMTPMail.plainTextPart context
      parts = catMaybes [plainBody, htmlBody]
  if length parts == 0
    then return ()
    else sendEmail [email] subject parts

sendRegistrationCreatedAnalyticsMail :: String -> String -> Email -> AppContextM ()
sendRegistrationCreatedAnalyticsMail uName uSurname uEmail = do
  dswConfig <- asks _appContextConfig
  htmlTemplate <- liftIO $ parseGingerFile mLoadFile "templates/mail/registrationCreatedAnalytics.html.j2"
  plainTemplate <- liftIO $ parseGingerFile mLoadFile "templates/mail/registrationCreatedAnalytics.txt.j2"
  let analyticsAddress = dswConfig ^. analytics . email
      mailName = dswConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": New user"
      context = fromList [("uName", uName), ("uSurname", uSurname), ("uEmail", uEmail), ("mailName", mailName)]
      htmlBody = makeMailPart htmlTemplate SMTPMail.htmlPart context
      plainBody = makeMailPart plainTemplate SMTPMail.plainTextPart context
      parts = catMaybes [plainBody, htmlBody]
  if length parts == 0
    then return ()
    else sendEmail [analyticsAddress] subject parts

sendResetPasswordMail :: Email -> U.UUID -> String -> AppContextM ()
sendResetPasswordMail email userId hash = do
  dswConfig <- asks _appContextConfig
  htmlTemplate <- liftIO $ parseGingerFile mLoadFile "templates/mail/resetPassword.html.j2"
  plainTemplate <- liftIO $ parseGingerFile mLoadFile "templates/mail/resetPassword.txt.j2"
  let clientAddress = dswConfig ^. clientConfig . address
      resetLink = clientAddress ++ "/forgotten-password/" ++ U.toString userId ++ "/" ++ hash
      mailName = dswConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": Reset Password"
      context = fromList [("resetLink", resetLink), ("mailName", mailName)]
      htmlBody = makeMailPart htmlTemplate SMTPMail.htmlPart context
      plainBody = makeMailPart plainTemplate SMTPMail.plainTextPart context
      parts = catMaybes [plainBody, htmlBody]
  if length parts == 0
    then return ()
    else sendEmail [email] subject parts

-- --------------------------------
-- PRIVATE
-- --------------------------------
makeMailPart (Right template) maker context = Just . maker . TL.fromStrict $ render template context
makeMailPart (Left _) _ _ = Nothing

makeConnection :: Integral i => Bool -> String -> Maybe i -> ((SMTP.SMTPConnection -> IO a) -> IO a)
makeConnection False host Nothing = SMTP.doSMTP host
makeConnection False host (Just port) = SMTP.doSMTPPort host (fromIntegral port)
makeConnection True host Nothing = SMTPSSL.doSMTPSSL host
makeConnection True host (Just port) = SMTPSSL.doSMTPSSLWithSettings host settings
  where
    settings = SMTPSSL.defaultSettingsSMTPSSL {SMTPSSL.sslPort = fromIntegral port}

sendEmail :: [Email] -> TL.Text -> [MIME.Part] -> AppContextM ()
sendEmail to subject parts = do
  dswConfig <- asks _appContextConfig
  let mailConfig = dswConfig ^. mail
      from = mailConfig ^. email
      addrFrom = MIME.Address (Just . T.pack $ mailConfig ^. name) (T.pack from)
      addrsTo = map (MIME.Address Nothing . T.pack) to
      addrsCc = []
      addrsBcc = []
      mailHost = mailConfig ^. host
      mailPort = mailConfig ^. port
      mailSSL = mailConfig ^. ssl
      mailUsername = mailConfig ^. username
      mailPassword = mailConfig ^. password
      mailSubject = TL.toStrict subject
      mailMessage = SMTPMail.simpleMail addrFrom addrsTo addrsCc addrsBcc mailSubject parts
      callback connection = do
        authSuccess <- SMTP.authenticate Auth.LOGIN mailUsername mailPassword connection
        renderedMail <- MIME.renderMail' mailMessage
        if authSuccess
          then do
            SMTP.sendMail from to (S.concat . B.toChunks $ renderedMail) connection
            return . Right $ to
          else return . Left $ _ERROR_SERVICE_MAIL__AUTH_ERROR_MESSAGE
      errorCallback exc = return . Left . show $ (exc :: SomeException)
      runMailer = makeConnection mailSSL mailHost mailPort callback
  if mailConfig ^. enabled
    then do
      result <- liftIO $ catch runMailer errorCallback
      case result of
        Right recipients -> logInfo $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__EMAIL_SENT_OK recipients)
        Left excMsg -> logError $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__EMAIL_SENT_FAIL excMsg)
    else return ()
