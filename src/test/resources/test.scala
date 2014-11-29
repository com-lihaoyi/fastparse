package docs.camel

import akka.actor.{ Props, ActorSystem }
import akka.camel.CamelExtension

import language.postfixOps
import akka.util.Timeout

object Introduction {
  def foo(): Unit = {
    //#Consumer-mina
    import akka.camel.{ CamelMessage, Consumer }

    class MyEndpoint extends Consumer {
      def endpointUri = "mina2:tcp://localhost:6200?textline=true"

      def receive = {
        case msg: CamelMessage => { /* ... */ }
        case _                 => { /* ... */ }
      }
    }

    // start and expose actor via tcp
    import akka.actor.{ ActorSystem, Props }

    val system = ActorSystem("some-system")
    val mina = system.actorOf(Props[MyEndpoint])
    //#Consumer-mina
  }
  def bar(): Unit = {
    //#Consumer
    import akka.camel.{ CamelMessage, Consumer }

    class MyEndpoint extends Consumer {
      def endpointUri = "jetty:http://localhost:8877/example"

      def receive = {
        case msg: CamelMessage => { /* ... */ }
        case _                 => { /* ... */ }
      }
    }
    //#Consumer
  }
  def baz(): Unit = {
    //#Producer
    import akka.actor.Actor
    import akka.camel.{ Producer, Oneway }
    import akka.actor.{ ActorSystem, Props }

    class Orders extends Actor with Producer with Oneway {
      def endpointUri = "jms:queue:Orders"
    }

    val sys = ActorSystem("some-system")
    val orders = sys.actorOf(Props[Orders])

    orders ! <order amount="100" currency="PLN" itemId="12345"/>
    //#Producer
  }


}