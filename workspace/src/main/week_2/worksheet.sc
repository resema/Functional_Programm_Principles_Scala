class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int): Int = {
    if (b==0) a else gcd(b,a%b)
  }

  def numer = x
  def denom = y

  def less(that: Rational) = {
    numer * that.denom < that.numer * denom
  }

  def max(that: Rational) = {
    if (this.less(that)) that else this
  }

  def + (that: Rational) = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  }

  def neg: Rational = new Rational(-numer, denom)

  def sub(that: Rational): Rational = {
    this + that.neg
  }

  override def toString = {
    numer / gcd(numer, denom) + "/" + denom / gcd(numer, denom)
  }
}


val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

y + y

new Rational(2)
