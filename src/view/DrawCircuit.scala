package view

import com.mxgraph.util.mxConstants
import com.mxgraph.view.mxGraph
import model._

/**
 * Created by joshua on 08/04/15.
 */
case class DrawCircuit (val graph : mxGraph) {
  val nodes = scala.collection.mutable.Stack[AnyRef]()
  val deltaY = 50
  val deltaX = 25
  val lastSources = scala.collection.mutable.MutableList[AnyRef]()
  val lastDrains = scala.collection.mutable.MutableList[AnyRef]()
  var parent = graph.getDefaultParent
  var currentX = 0;
  var sourcesYLimit = 0;
  var drainsYLimit = 0;
  var maxX = 0;

  def draw () : Unit = {
    parent = graph.getDefaultParent
    // No gates are currently drawn, so reset
    Result.resetDrawnGates()
    styleSheet("nmos_de")
    styleSheet("pmos_de")
    styleSheet("nmos_en")
    styleSheet("pmos_en")
    wireStyle(true)
    wireStyle(false)
    otherWireStyle()
    graph.getModel.beginUpdate()
    try { {
      val node = graph.insertVertex(parent, null, "output", 0, 0, 0, 1)
      val sources = Result.getSources
      // every gate in sources should connect to the output wire
      for (sources <- sources) {
        nodes push node
      }
      currentX = 0
      drawNetwork(sources, 0, true)
      maxX = currentX
      currentX = -10
      val drains = Result.getDrains
      for (drain <- drains) {
        nodes push node
      }
      drawNetwork(drains, 0, false)
      maxX = Math.max(maxX, currentX)
      val newOut = graph.insertVertex(parent, null, "output", -15, 0, maxX + 30, 1, "wireUndriven")

      val drain = graph.insertVertex(parent, null, "drain", -15, drainsYLimit, maxX + 30, 1,
                                     "wireDriven")

      val source = graph.insertVertex(parent, null, "source", -15, -1.0 * sourcesYLimit, maxX + 30, 1,
                                      "wireDriven")


      if (Result.get() == High()) {
        graph.setCellStyle("wireDriven", Array(newOut))
      }

      for (edge <- graph.getEdges(node)) {
        graph.splitEdge(edge, Array(newOut))
      }

      for (sourceGate <- lastSources) {
        val x = graph.getCellGeometry(sourceGate).getCenterX
        val dot = graph.insertVertex(parent, null, "", x, -1.0 * sourcesYLimit, 1, 1)
        graph.insertEdge(parent, null, "", sourceGate, dot, "wireOther")
      }

      for (drainGate <- lastDrains) {
        val x = graph.getCellGeometry(drainGate).getCenterX
        val dot = graph.insertVertex(parent, null, "", x, drainsYLimit, 1, 1)
        graph.insertEdge(parent, null, "", drainGate, dot, "wireOther")
      }

      lastSources.clear()
      lastDrains.clear()

      graph.removeCells(Array(node))
    }
    } finally {
      graph.getModel.endUpdate()
    }
  }

  private def styleSheet (name : String) : Unit = {
    // add gate as node at (xPos, y+1)
    // get graph stylesheet
    val stylesheet = graph.getStylesheet

    // define image style name
    val styleName = name

    // define image style
    val style = new java.util.HashMap[String, AnyRef]()
    style.put(mxConstants.STYLE_SHAPE, mxConstants.SHAPE_IMAGE)
    style.put(mxConstants.STYLE_IMAGE, "file:resources/%s.png".format(name))
    style.put(mxConstants.STYLE_LABEL_POSITION, mxConstants.ALIGN_LEFT)

    stylesheet.putCellStyle(styleName, style)
  }

  private def otherWireStyle() : Unit = {
    // get graph stylesheet
    val stylesheet = graph.getStylesheet

    // define image style name
    val styleName = "wireOther"

    // define image style
    val style = new java.util.HashMap[String, AnyRef]()
    style.put(mxConstants.STYLE_ENDARROW, mxConstants.NONE)
    style.put(mxConstants.STYLE_STARTARROW, mxConstants.NONE)
    style.put(mxConstants.STYLE_STROKECOLOR, "red")
    style.put(mxConstants.STYLE_FILLCOLOR, "red")
    style.put(mxConstants.STYLE_LABEL_POSITION, mxConstants.ALIGN_RIGHT)

    stylesheet.putCellStyle(styleName, style)

  }

  private def wireStyle (driven : Boolean) : Unit = {
    // get graph stylesheet
    val stylesheet = graph.getStylesheet

    // define image style name
    val styleName = "wire" + (if (driven) {
      "Driven"
    } else {
      "Undriven"
    })

    // define image style
    val style = new java.util.HashMap[String, AnyRef]()
    style.put(mxConstants.STYLE_SHAPE, mxConstants.SHAPE_LINE)
    style.put(mxConstants.STYLE_STROKECOLOR, if (driven) {
      "red"
    } else {
      "black"
    })
    style.put(mxConstants.STYLE_FILLCOLOR, if (driven) {
      "red"
    } else {
      "black"
    })
    style.put(mxConstants.STYLE_LABEL_POSITION, mxConstants.ALIGN_RIGHT)

    stylesheet.putCellStyle(styleName, style)
  }

  private[this] def drawNetwork (gates : Array[Transistor], y : Int, drawingTopNetwork : Boolean) : Unit = {
    if (gates.isEmpty) {
      // if last node on the chain, nothing to connect it to, so forget it
      if (drawingTopNetwork) {
        lastSources += nodes.pop()
        sourcesYLimit = scala.math.max(sourcesYLimit, y)
      } else {
        lastDrains += nodes.pop()
        drainsYLimit = scala.math.max(drainsYLimit, y)
      }
    } else {
      for (gate <- gates) {
        gate.drawnGate match {
          case Some(node : AnyRef) => {
            val box = graph.getCellGeometry(node)
            val x = box.getCenterX - deltaX * 2
            val tempy = box.getCenterY + (box.getHeight * (if (drawingTopNetwork) {
              -0.5
            } else {
              0.5
            })) - deltaY
            val point = graph insertVertex(parent, null, "", x, tempy, 0, 0)
            graph.insertEdge(parent, null, "", nodes.pop(), node,
                             mxConstants.STYLE_SHAPE + "=" + mxConstants.SHAPE_LINE)
          }
          case None => {
            val previousNode = nodes.pop()
            val node =
              graph.insertVertex(parent, null, gate.input.toString, currentX, if (drawingTopNetwork) {
                -(y + deltaY)
              } else {
                y
              }, deltaX * 2.0, deltaY, (if (!drawingTopNetwork) {
                "nmos"
              } else {
                "pmos"
              }) + (if (gate.get() == Undriven()) {
                "_de"
              } else {
                "_en"
              }))
            gate.drawnGate = Some(node)
            graph insertEdge
            (parent, null, "", previousNode, node, mxConstants.STYLE_SHAPE + "=" + mxConstants.SHAPE_IMAGE)
            nodes push node
            drawNetwork(if (drawingTopNetwork) {
              gate.source.getSources
            } else {
              gate.drain.getDrains
            }, y + deltaY, drawingTopNetwork)
            currentX += deltaX * 2
          }
        }
      }
    }
  }
}
