module Service.Mail.Mailer
  ( sendRegistrationConfirmationMail
  , sendRegistrationCreatedAnalyticsMail
  , sendResetPasswordMail
  ) where

import Control.Exception (SomeException, catch, handle)
import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.Either (rights)
import Data.HashMap.Strict (HashMap, fromList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as U
import qualified Network.HaskellNet.Auth as Auth
import qualified Network.HaskellNet.SMTP as SMTP
import qualified Network.HaskellNet.SMTP.SSL as SMTPSSL
import qualified Network.Mail.Mime as MIME
import qualified Network.Mail.SMTP as SMTPMail
import qualified Network.Mime as MIME
import System.Directory (listDirectory)
import System.FilePath (takeFileName)

import Constant.Component
import Constant.Mailer
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.User.User
import Util.Logger
import Util.Template (loadAndRender)

sendRegistrationConfirmationMail :: User -> String -> AppContextM ()
sendRegistrationConfirmationMail user hash = do
  dswConfig <- asks _appContextConfig
  let clientAddress = dswConfig ^. clientConfig . address
      activationLink = clientAddress ++ "/signup-confirmation/" ++ U.toString (user ^. uuid) ++ "/" ++ hash
      mailName = dswConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": Confirmation Email"
      additionals = [("activationLink", (Aeson.String $ T.pack activationLink))]
      context = makeMailContext mailName clientAddress user additionals
  parts <- loadMailTemplateParts _MAIL_REGISTRATION_REGISTRATION_CONFIRMATION context
  sendEmail [user ^. email] subject parts

sendRegistrationCreatedAnalyticsMail :: User -> AppContextM ()
sendRegistrationCreatedAnalyticsMail user = do
  dswConfig <- asks _appContextConfig
  let clientAddress = dswConfig ^. clientConfig . address
      analyticsAddress = dswConfig ^. analytics . email
      mailName = dswConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": New user"
      context = makeMailContext mailName clientAddress user []
  parts <- loadMailTemplateParts _MAIL_REGISTRATION_CREATED_ANALYTICS context
  sendEmail [analyticsAddress] subject parts

sendResetPasswordMail :: User -> String -> AppContextM ()
sendResetPasswordMail user hash = do
  dswConfig <- asks _appContextConfig
  let clientAddress = dswConfig ^. clientConfig . address
      resetLink = clientAddress ++ "/forgotten-password/" ++ U.toString (user ^. uuid) ++ "/" ++ hash
      mailName = dswConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": Reset Password"
      additionals = [("resetLink", (Aeson.String $ T.pack resetLink))]
      context = makeMailContext mailName clientAddress user additionals
  parts <- loadMailTemplateParts _MAIL_RESET_PASSWORD context
  sendEmail [user ^. email] subject parts

-- --------------------------------
-- PRIVATE
-- --------------------------------
type MailContext = HashMap T.Text Aeson.Value

makeHTMLPart fn context =
  liftIO $ do
    template <- loadAndRender fn context
    return $ (SMTPMail.htmlPart . TL.fromStrict) <$> template

makePlainTextPart fn context =
  liftIO $ do
    template <- loadAndRender fn context
    return $ (SMTPMail.plainTextPart . TL.fromStrict) <$> template

makeMailContext :: String -> String -> User -> [(T.Text, Aeson.Value)] -> MailContext
makeMailContext mailName clientAddress user others =
  fromList $
  [ ("mailName", Aeson.String $ T.pack mailName)
  , ("clientAddress", Aeson.String $ T.pack clientAddress)
  , ("user", Aeson.Object userMap)
  ] ++
  others
  where
    userMap :: MailContext
    userMap =
      fromList
        [ ("name", Aeson.String $ T.pack $ user ^. name)
        , ("surname", Aeson.String $ T.pack $ user ^. surname)
        , ("email", Aeson.String $ T.pack $ user ^. email)
        , ("role", Aeson.String $ T.pack $ user ^. role)
        , ("active", Aeson.Bool $ user ^. active)
        ]

loadMailTemplateParts :: String -> MailContext -> AppContextM [MIME.Part]
loadMailTemplateParts mailName context = do
  let root = _MAIL_TEMPLATE_ROOT ++ mailName ++ "/"
      commonRoot = _MAIL_TEMPLATE_ROOT ++ _MAIL_TEMPLATE_COMMON_FOLDER ++ "/"
  plainTextPart <- makePlainTextPart (root ++ _MAIL_TEMPLATE_PLAIN_NAME) context
  htmlPart <- makeHTMLPart (root ++ _MAIL_TEMPLATE_HTML_NAME) context
  let mainParts = rights [plainTextPart, htmlPart]
  case (htmlPart, plainTextPart) of
    (Left _, Right _) -> logWarn $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__MISSING_HTML mailName)
    (Right _, Left _) -> logWarn $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__MISSING_PLAIN mailName)
    (Left _, Left _) -> logError $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__MISSING_HTML_PLAIN mailName)
    (_, _) -> return ()
  if length mainParts > 0
    then do
      templateFileParts <- loadFileParts $ root ++ _MAIL_TEMPLATE_ATTACHMENTS_FOLDER
      globalFileParts <- loadFileParts $ commonRoot ++ _MAIL_TEMPLATE_ATTACHMENTS_FOLDER
      return $ mainParts ++ templateFileParts ++ globalFileParts
    else return []

loadFileParts :: String -> AppContextM [MIME.Part]
loadFileParts root =
  liftIO $ handle (\(_ :: SomeException) -> return []) $ do
    files <- listDirectory root
    fileParts <- mapM loadFilePart $ map (\fn -> root ++ "/" ++ fn) files
    return $ rights fileParts

loadFilePart :: String -> IO (Either String MIME.Part)
loadFilePart filename =
  handle (\(e :: SomeException) -> return . Left . show $ e) $ do
    let contentType = E.decodeUtf8 $ MIME.defaultMimeLookup (T.pack filename)
        cidHeader = ("Content-ID", T.pack $ "<" ++ takeFileName filename ++ ">")
    filePart <- SMTPMail.filePart contentType filename
    return . Right $ filePart {MIME.partHeaders = (MIME.partHeaders filePart) ++ [cidHeader]}

makeConnection :: Integral i => Bool -> String -> Maybe i -> ((SMTP.SMTPConnection -> IO a) -> IO a)
makeConnection False host Nothing = SMTP.doSMTP host
makeConnection False host (Just port) = SMTP.doSMTPPort host (fromIntegral port)
makeConnection True host Nothing = SMTPSSL.doSMTPSSL host
makeConnection True host (Just port) = SMTPSSL.doSMTPSSLWithSettings host settings
  where
    settings = SMTPSSL.defaultSettingsSMTPSSL {SMTPSSL.sslPort = fromIntegral port}

sendEmail :: [Email] -> TL.Text -> [MIME.Part] -> AppContextM ()
sendEmail to subject [] = return () -- empty mail won't be sent
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
