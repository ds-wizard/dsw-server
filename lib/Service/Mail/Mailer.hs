module Service.Mail.Mailer
  ( sendRegistrationConfirmationMail
  , sendRegistrationCreatedAnalyticsMail
  , sendResetPasswordMail
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as U
import qualified Network.HaskellNet.Auth as Auth
import qualified Network.HaskellNet.SMTP as SMTP
import qualified Network.HaskellNet.SMTP.SSL as SMTPSSL
import qualified Network.Mail.Mime as MIME

import LensesConfig
import Model.Context.AppContext
import Model.User.User

createEmail to from subject plainBody "" _ = MIME.simpleMail' to from subject plainBody
createEmail to from subject plainBody htmlBody attachments =
  MIME.simpleMailInMemory to from subject plainBody htmlBody attachments

makeConnection False host Nothing = SMTP.doSMTP host
makeConnection False host (Just port) = SMTP.doSMTPPort host (fromIntegral port)
makeConnection True host Nothing = SMTPSSL.doSMTPSSL host
makeConnection True host (Just port) = SMTPSSL.doSMTPSSLWithSettings host settings
  where
    settings = SMTPSSL.defaultSettingsSMTPSSL {SMTPSSL.sslPort = fromIntegral port}

sendEmail :: Email -> TL.Text -> TL.Text -> AppContextM ()
sendEmail to subject body = do
  dswConfig <- asks _appContextConfig
  let mailConfig = dswConfig ^. mail
      from = mailConfig ^. email
      addrFrom = MIME.Address (Just . T.pack $ mailConfig ^. name) (T.pack from)
      addrTo = MIME.Address Nothing (T.pack to)
      plainBody = ""
      htmlBody = body
      mailHost = mailConfig ^. host
      mailPort = mailConfig ^. port
      mailSSL = mailConfig ^. ssl
      mailUsername = mailConfig ^. username
      mailPassword = mailConfig ^. password
      mailSubject = TL.toStrict subject
      mailMessage = createEmail addrFrom addrTo mailSubject plainBody htmlBody []
  if mailConfig ^. enabled
    then liftIO $ makeConnection mailSSL mailHost mailPort $ \connection -> do
           authSuccess <- SMTP.authenticate Auth.LOGIN mailUsername mailPassword connection
           renderedMail <- MIME.renderMail' mailMessage
           if authSuccess
             then SMTP.sendMail from [to] (S.concat . B.toChunks $ renderedMail) connection
             else return ()
    else return ()

sendRegistrationConfirmationMail :: Email -> U.UUID -> String -> AppContextM ()
sendRegistrationConfirmationMail email userId hash = do
  dswConfig <- asks _appContextConfig
  let clientAddress = dswConfig ^. clientConfig . address
      clientLink = clientAddress ++ "/signup-confirmation/" ++ U.toString userId ++ "/" ++ hash
      link = "<a href=\"" ++ clientLink ++ "\">here</a>"
      mailName = dswConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": Confirmation Email"
      body = TL.pack $ "Hi! For account activation you have to click " ++ link ++ "! " ++ mailName ++ " Team"
  sendEmail email subject body

sendRegistrationCreatedAnalyticsMail :: String -> String -> Email -> AppContextM ()
sendRegistrationCreatedAnalyticsMail uName uSurname uEmail = do
  dswConfig <- asks _appContextConfig
  let analyticsAddress = dswConfig ^. analytics . email
      mailName = dswConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": New user"
      body =
        TL.pack $ "Hi! We have a new user (" ++ uName ++ " " ++ uSurname ++ ", " ++ uEmail ++ ") in our Wizard! " ++
        mailName ++
        " Team"
  sendEmail analyticsAddress subject body

sendResetPasswordMail :: Email -> U.UUID -> String -> AppContextM ()
sendResetPasswordMail email userId hash = do
  dswConfig <- asks _appContextConfig
  let clientAddress = dswConfig ^. clientConfig . address
      clientLink = clientAddress ++ "/forgotten-password/" ++ U.toString userId ++ "/" ++ hash
      link = "<a href=\"" ++ clientLink ++ "\">here</a>"
      mailName = dswConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": Reset Password"
      body = TL.pack $ "Hi! You can set up a new password " ++ link ++ "! " ++ mailName ++ " Team"
  sendEmail email subject body
