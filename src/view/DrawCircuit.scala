package view

import com.mxgraph.util.mxConstants
import com.mxgraph.view.mxGraph
import model._

/**
 * Created by joshua on 08/04/15.
 */
case class DrawCircuit(val graph : mxGraph) {
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

  def draw() : Unit = {
    parent = graph.getDefaultParent
    graph.getModel.beginUpdate()
    try {
      val node = graph.insertVertex(parent, null, "output", 0, 0, 0, 1)
      val sources = Result.getSources
      // every gate in sources should connect to the output wire
      for (sources <- sources) nodes push node
      currentX = 0
      drawNetwork(sources, 0, true)
      maxX = currentX
      currentX = -10
      val drains = Result.getDrains
      for (drain <- drains) nodes push node
      drawNetwork(drains, 0, false)
      maxX = Math.max(maxX, currentX)
      val newOut = graph.insertVertex(parent, null, "output", -15, 0, maxX + 30, 1, mxConstants.STYLE_SHAPE + "=" +
        mxConstants.SHAPE_LINE)

      val drain = graph.insertVertex(parent, null, "drain", -15, drainsYLimit + 2*deltaY, maxX + 30, 1, mxConstants
        .STYLE_SHAPE + "=" + mxConstants.SHAPE_LINE)

      val source = graph.insertVertex(parent, null, "source", -15, -1.0 * (sourcesYLimit + deltaY), maxX + 30, 1, mxConstants
        .STYLE_SHAPE + "=" + mxConstants.SHAPE_LINE)


      if (Result.get() != Undriven()) {
        graph.setCellStyle("fillColor=green", Array(node))
        graph.setCellStyle(mxConstants.STYLE_FONTSTYLE + "=" + mxConstants.FONT_BOLD, Array(node))
      }

      for (edge <- graph.getEdges(node)) {
        graph.splitEdge(edge, Array(newOut))
      }

      for (sourceGate <- lastSources) {
        graph.insertEdge(parent, null, "", sourceGate, source)
      }

      for (drainGate <- lastDrains) {
        graph.insertEdge(parent, null, "", drainGate, drain)
      }

      lastSources.clear()
      lastDrains.clear()

      graph.removeCells(Array(node))
    } finally {
      graph.getModel.endUpdate()
    }
  }

  private[this] def drawNetwork(gates : Array[Gate], y : Int, drawingTopNetwork : Boolean) : Unit = {
    val yPos = y
    if (gates.isEmpty) {
      // if last node on the chain, nothing to connect it to, so forget it
      if (drawingTopNetwork) {
        lastSources += nodes.pop()
        sourcesYLimit = scala.math.max(sourcesYLimit, y)
      } else {
        lastDrains += nodes.pop()
        sourcesYLimit = scala.math.max(drainsYLimit, y)
      }
    } else for (gate <- gates) {
      // add gate as node at (xPos, y+1)
      val previousNode = nodes.pop()
      val node =
        graph.insertVertex(parent, null, gate.input.toString +
          (if (drawingTopNetwork) {
            "sources"
          } else {
            "drains"
          }), currentX,
          if (!drawingTopNetwork) {
            y
          } else {
            -(y + deltaY)
          }, deltaX, deltaY)
      graph insertEdge(parent, null, "", previousNode, node, mxConstants.STYLE_SHAPE + "=" + mxConstants
        .SHAPE_IMAGE)

      nodes push node
      if (gate.get() != Undriven()) {
        graph.setCellStyle("fillColor=green", Array(node))
      }

      drawNetwork(if (drawingTopNetwork) {
        gate.source.getSources
      } else {
        gate.drain.getDrains
      }, yPos + deltaY, drawingTopNetwork)
      currentX += deltaX * 2
    }
  }
}
