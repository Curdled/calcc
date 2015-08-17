package calcc

/**
 * Created by joe on 11/08/15.
 */
abstract class PToken

case class PBPlus(left: PToken, right: PToken) extends PToken
case class PBMinus(left: PToken, right: PToken) extends PToken
case class PUPlus(token: PToken) extends PToken
case class PUMinus(token: PToken) extends PToken
case class PBMulti(left: PToken, right: PToken) extends PToken
case class PUCos(token: PToken) extends PToken
case class PUFact(token: PToken) extends PToken
case class PNum(value:List[Char]) extends PToken

abstract class Sum

case class DSum(Sum: Sum, minus: Minus)

abstract class Minus