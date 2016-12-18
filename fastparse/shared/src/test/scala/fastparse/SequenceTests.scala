package fastparse

import fastparse.core.Logger
import utest._

/**
 *
 */
object SequenceTests extends TestSuite {

  val tests = TestSuite {

    sealed abstract class Part
    case class Text(s: String) extends Part
    case class Number(n: String) extends Part
    case object NewLine extends Part
    case class PostalAddress(country: String, postalCode: String, city: String,
                             streetName: String, streetNumber: String, name: Any)

    object allPart extends fastparse.sequence.allVector[Part]

    'postalAddress{
      import allPart._
      import allPart.Parsed

      val countries: Seq[String] = Seq("CHINA", "FRANCE", "GERMANY", "JAPAN",
        "UNITED-KINGDOM")
      'ElemIn{
        val elemInCountries: Parser[Unit] = P(ElemIn(countries.map(Text)))

        val Parsed.Success(r, _) = elemInCountries.parse(Vector(Text("CHINA")))

        val Parsed.Failure(_, 0, _) = elemInCountries.parse(Vector(Number("1")))

        val Parsed.Failure(_, 0, _) = elemInCountries.parse(Vector(Text("USA")))
      }

      val nl: Parser[Unit] = ElemIn(Seq(NewLine))
      val number: Parser[String] = F({ case Number(v) => v })
      val text: Parser[String] = F({ case Text(v) => v })
      val country: Parser[String] = F({
        case Text(v) if countries.contains(v.toUpperCase) =>
          v.toUpperCase
      })

      'PartialFunction{
        val Parsed.Success(r, _) = country.parse(Vector(Text("germany")))
        assert(r == "GERMANY")

        val Parsed.Failure(_, 0, _) = country.parse(Vector(Number("1")))

        val Parsed.Failure(_, 0, _) = country.parse(Vector(Text("USA")))
      }

      val name: Parser[Any] = P((!nl ~ (number | text)).rep(1)).map(_.mkString(" "))
      val street: Parser[(String, String)] = P((
        (number.filter(_.length < 5) ~ text).map(_.swap) |
        (text ~ number.filter(_.length < 5))) ~ nl)

      'Either{
        val Parsed.Success(r, _) = street.parse(Vector(Number("456"), Text("Central Park"), NewLine))
        assert(r == ("Central Park", "456"))

        val Parsed.Success(r2, _) = street.parse(Vector(Text("Central Park"), Number("456"), NewLine))
        assert(r2 == ("Central Park", "456"))

        val Parsed.Failure(_, _, _) = street.parse(Vector(Number("456"), Number("789"), NewLine))

        val Parsed.Failure(_, _, _) = street.parse(Vector(Text("Central Park"), Text("456")))

        val Parsed.Failure(_, _, _) = street.parse(Vector(Number("Central Park"), Text("456"), Text("789"), NewLine))
      }

      val postalCodeAndCity: Parser[(String, String)] = P((
        (number.filter(_.length > 4) ~ text) |
        (text ~ number.filter(_.length > 4)).map(_.swap)) ~ nl)

      val occidentalPostalAddress: Parser[PostalAddress] = P(
        name ~ nl ~ street ~ postalCodeAndCity ~ country).
        map(l => PostalAddress(l._4, l._3._1, l._3._2, l._2._1, l._2._2, l._1))

      val orientalPostalAddress: Parser[PostalAddress] = P(
        country ~ nl ~ postalCodeAndCity ~ street ~ name).
        map(l => PostalAddress(l._1, l._2._1, l._2._2, l._3._1, l._3._2, l._4))

      val fullPostalAddress = P(orientalPostalAddress | occidentalPostalAddress)

      'completeResult{
        val parsed = Vector(
          Text("Scala programming"), NewLine,
          Number("123"), Text("rue des Champs Elysees"), NewLine,
          Number("75000"), Text("PARIS"), NewLine,
          Text("FRANCE"))

        val Parsed.Success(r, _) = fullPostalAddress.parse(parsed, 0)
        assert(r == PostalAddress("FRANCE", "75000", "PARIS", "rue des Champs Elysees", "123", "Scala programming"))
      }
    }

    /**
     * Assumes we already recognized with a String parser
     * and we get a Seq or Vector of amount, date or person
     */

    sealed abstract class Input
    case class Amount(sum: Double) extends Input
    case class Date(y: Int, m: Int, d: Int) extends Input
    case class Person(name: String) extends Input

    sealed abstract class Output
    case class Appointment(from: Date, to: Date, participants: Seq[Person]) extends Output
    case class PartialDebt(date: Option[Date], amount: Option[Amount], to: Option[Person]) extends Output

    object allInput extends fastparse.sequence.allVector[Input]

    'recognizeEntry{
      import allInput._
      import allInput.Parsed

      val date: Parser[Date] = F({ case d: Date => d })

      'PartialFunction0{
        val Parsed.Success(Date(_, _, _), _) = date.parse(Vector(Date(1, 2, 3)))
        val Parsed.Failure(_, _, _) = date.parse(Vector(Amount(0)))
      }

      val amount: Parser[Amount] = F({ case a: Amount => a })
      val person: Parser[Person] = F({ case p: Person => p })
      val persons: Parser[Seq[Person]] = P(person.rep(1))

      'PartialFunctionRep{
        val Parsed.Success(Seq(Person("a"), Person("b")), _) = persons.parse(Vector(Person("a"), Person("b")))
        val Parsed.Success(Seq(Person("a"), Person("b")), _) = persons.parse(Vector(Person("a"), Person("b"), Date(1, 2, 3)))
        val Parsed.Success(Seq(Person("a")), _) = persons.parse(Vector(Person("a"), Date(1, 2, 3)))
        val Parsed.Failure(_, _, _) = persons.parse(Vector(Amount(0)))
      }

      val appointmentDateFirst: Parser[(Date, Date, Seq[Person])] = P(
        (date ~ ((date ~ person.rep(1)) |
          (person.rep(1) ~ date)))).map({
          case (d1: Date, (d2: Date, p: Seq[Person])) => (d1, d2, p)
          case (d1: Date, (p: Seq[Person], d2: Date)) => (d1, d2, p)
        })

      val appointmentPersonFirst: Parser[(Date, Date, Seq[Person])] = P(person.rep(1) ~ date ~ date).map(t => (t._2, t._3, t._1))

      val appointment: Parser[Appointment] = P(appointmentDateFirst | appointmentPersonFirst).map(x => Appointment.tupled(x))

      /**
       * Just a crazy test to see where we can go with fastparse !
       *
       * Otherwise a structure like appointment is better
       */
      def debtPart(avoid: Parser[_]): Parser[Seq[Input]] = P(!avoid ~ (
        (date ~ debtPart(date | avoid).?) |
        (amount ~ debtPart(amount | avoid).?) |
        (person ~ debtPart(person | avoid).?))) map {
        case (x, y) => x +: y.getOrElse(Nil)
      }

      val debt: Parser[PartialDebt] = debtPart(Fail) map {
        case lst =>
          /**
           * We can also go in a mutable way with 'var' for each field of
           * PartialDebt and set each field (and avoids re-allocation) :
           *
           * val r = Mutable.PartialDebt(None, None, None)
           *
           * lst.foreach {
           *   case d: Date => r.date = Some(d)
           *   case a: Amount => r.amount = Some(a)
           *   case p: Person => r.to = Some(p)
           * }
           */
          lst.foldLeft(PartialDebt(None, None, None)) {
            case (acc, cur) => cur match {
              case d: Date   => acc.copy(date = Some(d))
              case a: Amount => acc.copy(amount = Some(a))
              case p: Person => acc.copy(to = Some(p))
            }
          }
      }

      val out = P(appointment | debt)

      'PartialDebt{
        val Parsed.Success(PartialDebt(Some(Date(1, 2, 3)), None, Some(Person("a"))), _) = out.parse(Vector(Person("a"), Date(1, 2, 3)))
      }

      'Appointment{
        val Parsed.Success(Appointment(Date(1, 2, 3), Date(4, 5, 6), Seq(Person("a"), Person("b"))), _) = out.parse(Vector(Person("a"), Person("b"), Date(1, 2, 3), Date(4, 5, 6)))
      }

    }
  }
}
