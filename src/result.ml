let is_error = function Ok _v -> false | Error _v -> true

let get_ok = function Ok v -> v | Error _v -> invalid_arg "Result.get_ok"
