package ghpages.examples

import cats.effect.{ConcurrentEffect, ContextShift, IO}
import fs2.concurrent.SignallingRef
import ghpages.GhPagesMacros
import japgolly.scalajs.react._
import vdom.html_<^._
import ghpages.examples.util.SideBySide

import scala.concurrent.ExecutionContext.Implicits.global

object TodoExample {

  def title = "Todo List"

  def content = SideBySide.Content(jsSource, source, main())

  lazy val main = addIntro(TodoApp.withKey(_)(model), _ (scalaPortOf("An Application")))

  val jsSource =
    """
      |class TodoList extends React.Component {
      |  render() {
      |    var createItem = (itemText,index) => React.createElement('li', {key: index}, itemText);
      |    return React.createElement('ul', null, this.props.items.map(createItem));
      |  }
      |}
      |
      |class TodoApp extends React.Component {
      |  constructor(props) {
      |    super(props);
      |    this.state = {items: [], text: ''};
      |    this.onChange = this.onChange.bind(this);
      |    this.handleSubmit = this.handleSubmit.bind(this);
      |  }
      |
      |  onChange(e) {
      |    this.setState({text: e.target.value});
      |  }
      |
      |  handleSubmit(e) {
      |    e.preventDefault();
      |    var nextItems = this.state.items.concat([this.state.text]);
      |    var nextText = '';
      |    this.setState({items: nextItems, text: nextText});
      |  }
      |
      |  render() {
      |    return (
      |      React.createElement("div", null,
      |        React.createElement("h3", null, "TODO"),
      |        React.createElement(TodoList, {items: this.state.items}),
      |        React.createElement("form", {onSubmit: this.handleSubmit},
      |          React.createElement("input", {onChange: this.onChange, value: this.state.text}),
      |          React.createElement("button", null, 'Add #' + (this.state.items.length + 1))
      |        )
      |      )
      |    );
      |  }
      |}
      |
      |ReactDOM.render(React.createElement(TodoApp), mountNode);
      |""".stripMargin

  val source =
    s"""
       |${GhPagesMacros.exampleSource}
       |
       |TodoApp().renderIntoDOM(mountNode)
       |""".stripMargin

  // EXAMPLE:START

  import com.rpiaggio.crystal.Flow
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val ioTimer = cats.effect.IO.timer
  implicit val ioCS: ContextShift[IO] = IO.contextShift(global)
  //  implicit val ioCE = cats.effect.ConcurrentEffect

  case class Model(items: SignallingRef[IO, List[String]] = SignallingRef[IO, List[String]](List.empty).unsafeRunSync())

  val model = Model()

  val TodoList = ScalaFnComponent[List[String]] { props =>
    def createItem(itemText: String) = <.li(itemText)

    <.ul(props map createItem: _*)
  }

  case class State(text: String)

  class Backend($: BackendScope[Model, State]) {
    def onChange(e: ReactEventFromInput) = {
      val newValue = e.target.value
      $.modState(_.copy(text = newValue))
    }

    def handleSubmit(model: Model, state: State)(e: ReactEventFromInput) =
      e.preventDefaultCB >>
        Callback {
          model.items.update(_ :+ state.text).unsafeRunSync()
        }
    //      $.modState(s => State(s.items :+ s.text, ""))

    val ItemList = Flow.flow(model.items.discrete)

    def render(model: Model, state: State) =
      <.div(
        <.h3("TODO"),
        ItemList { list =>
          TodoList(list.getOrElse(List.empty))
          //          TodoList(model.items.get.unsafeRunSync())
        },

        <.form(^.onSubmit ==> handleSubmit(model, state),
          <.input(^.onChange ==> onChange, ^.value := state.text),
          <.button("Add #", model.items.get.unsafeRunSync().length + 1)
        )
      )
  }

  val TodoApp = ScalaComponent.builder[Model]("TodoApp")
    .initialState(State(""))
    .renderBackend[Backend]
    .build

  // EXAMPLE:END
}
