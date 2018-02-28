module Query where

import Conllu.Type

data CField = Id | Form deriving (Eq,Show)
data Query = FieldQ CField | FieldValueQ CField String | AndQ [Query] | OrQ [Query] | NegQ Query
