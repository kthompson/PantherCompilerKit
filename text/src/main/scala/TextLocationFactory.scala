object TextLocationFactory {
  def empty(): TextLocation =
    new TextLocation(MakeSourceFile.empty(), new TextSpan(0, 0))
}
