import panther._

object Hex {
    def fromChar(curr: char): int =
        if (curr == '0') 0
        else if (curr == '1') 1
        else if (curr == '2') 2
        else if (curr == '3') 3
        else if (curr == '4') 4
        else if (curr == '5') 5
        else if (curr == '6') 6
        else if (curr == '7') 7
        else if (curr == '8') 8
        else if (curr == '9') 9
        else if (curr == 'a' || curr == 'A') 10
        else if (curr == 'b' || curr == 'B') 11
        else if (curr == 'c' || curr == 'C') 12
        else if (curr == 'd' || curr == 'D') 13
        else if (curr == 'e' || curr == 'E') 14
        else if (curr == 'f' || curr == 'F') 15
        else -1

    def from_string(s: string): int = from_string(s, 0, s.length)

    def from_string(s: string, index: int, len: int): int = {
        var value = fromChar(s(index))
        for (i <- (index + 1) to (index + len - 1)) {
            value = (value * 16) + fromChar(s(i))
        }
        value
    }
}