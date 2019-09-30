module Messages where
import Identifier
import Storage

data P2PMsg addr
  = Notify addr
  | PredecessorFound addr
  | PredecessorNotFound
  | GetPredecessor
  | GetSuccessors
  | Successors [addr]
  | FindSuccessor Identifier
  | FindSuccessorSuccess Identifier addr
  | FindSuccessorFailure Identifier
  | StoragePut Identifier Value
  | StorageGet Identifier
  | StorageGetSuccess Identifier Value
  | StoragePutSuccess Identifier
  | StorageFailure Identifier
  | DhtPut Identifier Value
  | DhtGet Identifier
  | DhtSuccess Identifier Value
  | DhtFailure Identifier
