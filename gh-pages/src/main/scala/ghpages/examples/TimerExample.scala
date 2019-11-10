package ghpages.examples

import cats.effect.{ContextShift, IO}
import ghpages.GhPagesMacros
import japgolly.scalajs.react._
import vdom.html_<^._

import scala.scalajs.js
import ghpages.examples.util.SideBySide
import japgolly.scalajs.react.component.Generic.UnmountedWithRoot
import japgolly.scalajs.react.vdom.VdomElement

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object TimerExample {

  def content = SideBySide.Content(jsSource, source, main())

  lazy val main = addIntro(Timer.withKey(_)(), _ (scalaPortOf("A Stateful Component")))

  val jsSource =
    """
      |class Timer extends React.Component {
      |  constructor(props) {
      |    super(props);
      |    this.state = {
      |      secondsElapsed: 0
      |    };
      |    this.tick = this.tick.bind(this);
      |  }
      |
      |  tick() {
      |    this.setState({secondsElapsed: this.state.secondsElapsed + 1});
      |  }
      |
      |  componentDidMount() {
      |    this.interval = setInterval(this.tick, 1000);
      |  }
      |
      |  componentWillUnmount() {
      |    clearInterval(this.interval);
      |  }
      |
      |  render() {
      |    return React.createElement("div", null, "Seconds Elapsed: ", this.state.secondsElapsed);
      |  }
      |}
      |
      |ReactDOM.render(React.createElement(Timer), mountNode);
      |""".stripMargin

  val source =
    s"""
       |${GhPagesMacros.exampleSource}
       |
       |Timer().renderIntoDOM(mountNode)
       |""".stripMargin

  // EXAMPLE:START

  implicit val ioTimer = cats.effect.IO.timer
  implicit val ioCS: ContextShift[IO] = IO.contextShift(global)
//  implicit val ioCE = cats.effect.ConcurrentEffect

  val stream: fs2.Stream[IO, Int] =
    fs2.Stream
      .iterateEval(0)(v => IO(v + 1))
      .covary[IO].metered(1 second)

  import com.rpiaggio.crystal.Flow

  val Seconds = Flow.flow(stream)

  val Timer = ScalaComponent.builder[Unit]("Timer")
    .render{ _ =>
      <.div("Seconds elapsed Crystal: ",
        Seconds { secs =>
          <.b(s"1: [${secs}]")
        },
        Seconds { secs =>
          <.b(s"2: [${secs}]")
        }
      )
    }
    .build

  // EXAMPLE:END
}
