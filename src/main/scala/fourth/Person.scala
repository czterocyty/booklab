package fourth

trait Validation[+E, +A] {
  def getOrElse[B >: A](default: => B): B
  def map[B](f: A => B): Validation[E, B]
  def flatMap[EE >: E, B](f: A => Validation[EE, B]): Validation[EE, B]
  def map2[EE >: E, B, C](o: Validation[EE, B])(f: (A, B) => C): Validation[EE, C]
  def violations(): List[E]
}
case class Valid[+A](v: A) extends Validation[Nothing, A] {
  override def getOrElse[B >: A](default: => B): B = {
    v
  }

  override def map[B](f: A => B): Validation[Nothing, B] = {
    Valid(f(v))
  }

  override def flatMap[EE >: Nothing, B](f: A => Validation[EE, B]): Validation[EE, B] = {
    f(v)
  }

  override def map2[EE >: Nothing, B, C](b: Validation[EE, B])(f: (A, B) => C): Validation[EE, C] = {
    flatMap(ea => b.map(eb => f(ea, eb)))
  }

  override def violations(): List[Nothing] = List()
}
case class Violations[+E](elist: List[E]) extends Validation[E, Nothing] {
  override def getOrElse[B >: Nothing](default: => B): B = {
    default
  }

  override def map[B](f: Nothing => B): Validation[E, B] = {
    Violations(elist)
  }

  override def flatMap[EE >: E, B >: Nothing](f: Nothing => Validation[EE, B]): Validation[EE, B] = {
    Violations(elist)
  }

  override def map2[EE >: E, B, C](o: Validation[EE, B])(f: (Nothing, B) => C): Validation[EE, C] = {
    o match {
      case Violations(olist) => Violations(elist ::: olist)
      case _ => Violations(this.elist)
    }
  }

  override def violations(): List[E] = elist
}

case class Person(name: Name, age: Age)
sealed case class Name(value: String)
sealed case class Age(value: Int)

object Person {
  def mkName(name: String): Validation[String, Name] =
    if (name == "" || name == null) Violations(List("Name is empty."))
    else Valid(Name(name))

  def mkAge(age: Int): Validation[String, Age] =
    if (age < 0) Violations(List("Age is out of range."))
    else Valid(Age(age))

  def mkPerson(name: String, age: Int): Validation[String, Person] =
    mkName(name).map2(mkAge(age))(new Person(_, _))
}