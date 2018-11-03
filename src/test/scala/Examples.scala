package circuits

import Circuit.Syntax._

class Examples[P[_]: Circuit](var1: P[Boolean], var2: P[Boolean]){

  def ex1: P[Boolean] =
    (var1 or var2) and not(var1 and var2)

  def xor1 = ex1

  def xor2: P[Boolean] =
    (xor1 or var2) and not(xor1 and var2)

  def ex2: P[Boolean] =
    not(not(var1)) and (not(var2) or var1)

  def notnotV1: P[Boolean] =
    not(not(var1))

  def notV1: P[Boolean] =
    not(var1)

  def ex3 =
    not(not(var1 or var2) and var1)
}
