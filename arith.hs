type Info = String

dummyInfo :: Info
dummyInfo = ""

data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term
          deriving (Show)

isNumericVal :: Term -> Bool
isNumericVal (TmZero _) = True
isNumericVal t = case t of
  TmSucc _ t1 -> isNumericVal t1
  _ -> False

isVal :: Term -> Bool
isVal (TmTrue _) = True
isVal (TmFalse _) = True
isVal t | isNumericVal t = True
        | otherwise = False

eval' :: Term -> Maybe Term
eval' t = case t of
  TmIf _ (TmTrue _) t2 t3 -> Just t2
  TmIf _ (TmFalse _) t2 t3 -> Just t3
  TmIf fi t1 t2 t3 -> (\tm -> TmIf fi tm t2 t3) <$> eval' t1
  TmSucc fi t1 -> (TmSucc fi) <$> eval' t1
  TmPred _ (TmZero _) -> Just (TmZero dummyInfo)
  TmPred _ (TmSucc _ nv1) | isNumericVal nv1 -> Just nv1
  TmPred fi t1 -> (TmPred fi) <$> eval' t1
  TmIsZero _ (TmZero _) -> Just (TmTrue dummyInfo)
  TmIsZero _ (TmSucc _ nv1) | isNumericVal nv1 -> Just (TmFalse dummyInfo)
  TmIsZero fi t1 -> (TmIsZero fi) <$> eval' t1
  _ -> Nothing

eval :: Term -> Term
eval t = case eval' t of
  Just t' -> eval t'
  Nothing -> t

bigStepEval :: Term -> Term
bigStepEval t = case t of
  TmIf _ t1 t2 t3 -> case bigStepEval t1 of
    TmTrue _ -> bigStepEval t2
    TmFalse _ -> bigStepEval t3
    _ -> t
  TmSucc fi t1 -> case bigStepEval t1 of
    TmPred _ t1' -> t1'
    TmZero _ -> TmSucc fi (TmZero dummyInfo)
    TmSucc _ t1' -> TmSucc fi t1
    _ -> t
  TmPred fi t1 -> case bigStepEval t1 of
    TmZero _ -> TmZero dummyInfo
    TmSucc _ t1' -> t1'
    _ -> t
  TmIsZero fi t1 -> case bigStepEval t1 of
    TmZero _ -> TmTrue dummyInfo
    _ -> t
  _ -> t
