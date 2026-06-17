module DC.Error (
  ErrorDetail(..),
  ErrorContextFrame(..),
  AppError(..),
  err,
  newBaseError,
  throwBaseError,
  annotateErrorPure,
  AppM
) where
import Control.Monad.Except (ExceptT, MonadError (throwError, catchError))

data ErrorDetail 
  = ParseError String
  | EntityValidationError
  deriving (Show)

data ErrorContextFrame = ErrorContextFrame
  { errorAction :: String
  , errorData :: [(String, String)]
  } deriving (Show)

data AppError = AppError
  { errorDetail :: ErrorDetail 
  , errorContext :: [ErrorContextFrame]
  } deriving (Show)

err :: String -> [(String, String)] -> AppM a -> AppM a
err actionName dynamics action = 
  catchError action (\(AppError detail context) ->
    let newFrame = ErrorContextFrame 
          { errorAction = actionName 
          , errorData = dynamics 
          }
    in throwError (AppError 
    { errorDetail = detail
    , errorContext = newFrame : context 
    })
  )

newBaseError :: ErrorDetail -> AppError
newBaseError d = AppError
  { errorDetail = d
  , errorContext = []
  }

throwBaseError :: ErrorDetail -> AppM a
throwBaseError d = throwError $ newBaseError d

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f (Left e)  = Left (f e)
mapLeft _ (Right x) = Right x

annotateErrorPure :: ErrorContextFrame -> Either AppError a -> Either AppError a
annotateErrorPure frame = mapLeft (\(AppError detail context) -> AppError 
  { errorDetail = detail
  , errorContext = frame : context 
  })

type AppM = ExceptT AppError IO