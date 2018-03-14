package fourth

import org.scalatest.FunSpec

class PersonTest extends FunSpec {

  it("valid person") {
    val adam = Person.mkPerson("Adam", 18)

    assert(adam.getOrElse(Person(Name("fake"), Age(0))) == Person(Name("Adam"), Age(18)))
  }

  it("invalid person") {
    val adam = Person.mkPerson("", -1)

    assert(adam.getOrElse(Person(Name("fake"), Age(0))) == Person(Name("fake"), Age(0)))

    assert(adam.violations() == List("Name is empty.", "Age is out of range."))
  }
}
