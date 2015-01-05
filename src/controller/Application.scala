package controller

import model._
import helper._

import scala.swing.SimpleSwingApplication
import scala.swing._
import scala.swing.event._

object Application extends SimpleSwingApplication {
  private var cmos : Node = Constant(true)

  def top = new MainFrame {
    title = "CMOS Calculator"

    object textbox extends TextField {
      columns = 30
    }

    val button = new Button("Parse")
    // TODO: Find an image view for this
    val display = new Button {}
    val output = new CheckBox("out")
    output.enabled = false
    val checkboxes = new FlowPanel {
      contents += output
    }

    contents = new SplitPane(
      Orientation.Vertical,
      new SplitPane(
        Orientation.Horizontal,
        new FlowPanel {
          contents += textbox
          contents += button
        },
        checkboxes
      ),
      display
    )
    // Don't do anything until the user tells us to, even if they change the
    // textbox contents
    listenTo(button, checkboxes)
    for (c <- checkboxes.contents) listenTo(c)
    reactions += {
      case ButtonClicked(c : CheckBox) => {
        Variable.setValue(c.text, c.selected)
        output.selected = (cmos.get)
      }
      case ButtonClicked(button) => {
        Variable.clear
        checkboxes.contents.clear
        checkboxes.contents += output
        Parser.variableParser(textbox.text) match {
          case Some(y) => {
            display.text = y.toString()
            val identMap = Variable.getMap
            for ((n, v) <- identMap) {
              if (n != "out") {
                object checkbox extends CheckBox {
                  text = n
                  selected = v
                }
                listenTo(checkbox)
                checkboxes.contents += checkbox
              }
            }
            cmos = y
          }
          case None => display.text = "Parsing failed"
        }
      }
    }
  }
}
