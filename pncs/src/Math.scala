import scala.annotation.tailrec
import panther._

object Math {

    // TODO: this is a pretty poor implementation but we dont have double support atm
    def pow(base: int, exponent: int): int = {
        _pow(base, exponent, 1)
    }

    @tailrec
    def _pow(base: int, exponent: int, value: int): int = {
        if (exponent > 0) _pow(base, exponent - 1, value * base)
        else value
    }
}