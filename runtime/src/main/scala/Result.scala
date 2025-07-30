enum Result[+A, +B] {
  case Error(value: A)
  case Success(value: B)
}
