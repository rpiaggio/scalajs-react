package ghpages.examples

import cats.effect.{CancelToken, Concurrent, IO}
import ghpages.GhPagesMacros
import ghpages.examples.TimerExample.{ReactFlowProps, stream}
import japgolly.scalajs.react._
import vdom.html_<^._

import scala.scalajs.js
import ghpages.examples.util.SideBySide
import japgolly.scalajs.react.component.Generic.UnmountedWithRoot
import japgolly.scalajs.react.vdom.VdomElement

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

  import fs2.concurrent._

  //  val topic = new Topic[] {}


  type ReactFlowProps[A] = Option[A] => VdomElement
  type ReactFlowComponent[A] = CtorType.Props[ReactFlowProps[A], UnmountedWithRoot[ReactFlowProps[A], _, _, _]]

  import cats.effect._
  import cats.implicits._
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val ioTimer = IO.timer
  implicit val ioCS: ContextShift[IO] = IO.contextShift(global)
  //  implicit val ce: ConcurrentEffect[IO] = IO.ioConcurrentEffect

  def stream[F[_] : Sync : Timer]: fs2.Stream[F, Int] =
    fs2.Stream
      .iterateEval(0)(v => Sync[F].delay(v + 1))
      .covary[F].metered(1 second)

  val ioStream = stream[IO]

  def flow[ /*F[_] : Sync,*/ A](stream: fs2.Stream[IO, A], key: js.UndefOr[js.Any] = js.undefined): ReactFlowComponent[A] = {

    class Backend($: BackendScope[ReactFlowProps[A], Option[A]]) {

      val done = SignallingRef[IO, Boolean](false).unsafeRunSync()

      def willMount = Callback {
        stream
          .interruptWhen(done)
          .evalMap(v => Sync[IO].delay($.setState(Some(v)).runNow()))
          .compile.drain
          .unsafeRunAsyncAndForget()
      }

      def willUnmount = Callback {
        done.set(true).unsafeRunSync()
      }

      def render(pr: ReactFlowProps[A], v: Option[A]): VdomElement =
        <.div(
          pr(v),
          <.button(^.tpe := "button", "STOP!", ^.onClick --> willUnmount)
        )
    }

    ScalaComponent
      .builder[ReactFlowProps[A]]("FlowWrapper")
      .initialState(Option.empty[A])
      .renderBackend[Backend]
      .componentWillMount(_.backend.willMount)
      .componentWillUnmount(_.backend.willUnmount)
      .shouldComponentUpdatePure(scope => (scope.currentState ne scope.nextState) || (scope.currentProps ne scope.nextProps))
      .build
      .withRawProp("key", key)
  }

  //  def topic[F[_]]: Stream[F, Int] =
  //    Stream.eval(Topic[F, Int](0)).flatMap { topic =>
  //  }

//  implicit lazy val reactCallbackTimer = new cats.effect.Timer[CallbackTo] {
//    def clock: cats.effect.Clock[CallbackTo] = ???
//    def sleep(duration: FiniteDuration): CallbackTo[Unit] = ???
//  }

  implicit val ioTimer = cats.effect.IO.timer

  val stream: fs2.Stream[IO, Int] =
    fs2.Stream
      .iterateEval(0)(v => IO(v + 1))
      .covary[IO].metered(1 second)

  val Timer = ScalaComponent.builder[Unit]("Timer")
    .render{ _ =>
      <.div("Seconds elapsed: ",
        flow(stream) { secs =>
          <.b(s"[${secs}]")
        }
      )
    }
    .build

  // EXAMPLE:END
}
