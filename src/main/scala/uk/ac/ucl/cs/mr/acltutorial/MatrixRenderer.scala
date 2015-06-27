package uk.ac.ucl.cs.mr.acltutorial

import java.util.UUID

import org.sameersingh.htmlgen.{RawHTML, HTML}

/**
* @author riedelcastro
*/
object MatrixRenderer {

  case class Cell(row:Int, col:Int, value:Any) {
    override def toString = s"""{"row":$row, "col": $col, "value": ${value match {
      case s:String => "\"" + s + "\""
      case _ => value.toString
    }}}"""
  }
  case class RowLabel(row:Int, name:String) {
    override def toString = s"""{"row": $row, "name": "$name"}"""
  }
  case class ColLabel(col:Int, name:String) {
    override def toString = s"""{"col": $col, "name": "$name"}"""
  }
  case class Matrix(cells:Seq[Cell] = Nil, rowLabels:Seq[RowLabel] = Nil,
                    colLabels:Seq[ColLabel] = Nil, hRulers:Seq[Int] = Nil,
                    boxes:Seq[Box] = Nil) {
    override def toString = {
      s"""{"cells": [${cells.mkString(",")}],
         |"cols":[${colLabels.mkString(",")}],
         |"rows":[${rowLabels.mkString(",")}],
         |"boxes":[${boxes.mkString(",")}],
         |"hRulers":[${hRulers.mkString(",")}]}""".stripMargin
    }

    def +(that:Matrix) = Matrix(cells ++ that.cells, rowLabels ++ that.rowLabels,
      colLabels ++ that.colLabels, hRulers ++ that.hRulers, boxes ++ that.boxes)
  }

  def colEmbedding(col:Int, rowOffset:Int, values:Seq[Any]) = {
    val cells = for (row <- values.indices) yield Cell(row + rowOffset, col, values(row))
    val box = Box(rowOffset,col,1,values.length)
    Matrix(cells, boxes = Seq(box))
  }

  def rowEmbedding(row:Int, colOffset:Int, values:Seq[Any]) = {
    val cells = for (col <- values.indices) yield Cell(row, col + colOffset, values(col))
    val box = Box(row,colOffset,values.length,1)
    Matrix(cells, boxes = Seq(box))
  }


  case class Box(row:Int, col:Int, width:Int, height:Int) {
    override def toString: String =
      s"""{"row":$row, "col": $col, "width": $width, "height":$height}"""
  }

  case class Layout(cw:Int = 30, ch:Int = 30, rowHeaderSize:Int = 50, colHeaderSize:Int = 100, numCols:Int = 4, numRows:Int = 4)

  def render(matrices:Seq[Matrix], layout: Layout = Layout()):HTML = {

    val actions = matrices.mkString(",")

    val id = "matrix" + UUID.randomUUID().toString
    val html = s"""
      |<div id = "$id" class="matrix">
      |</div>
      |<script>
      |var cw = ${layout.cw},
      |    ch = ${layout.ch},
      |    rowHeaderSize = ${layout.rowHeaderSize},
      |    colHeaderSize = ${layout.colHeaderSize},
      |    numCols = ${layout.numCols},
      |    numRows = ${layout.numRows};
      |    boxPadding = 5;
      |
      |var width = numCols * cw + rowHeaderSize,
      |    height = numRows * ch + colHeaderSize,
      |    div = d3.select('#$id'),
      |    svg = div.append('svg')
      |        .attr('width', width)
      |        .attr('height', height);
      |
      |var textOffsetY = ch / 2
      |
      |var cells = svg.append("g");
      |var rows = svg.append("g");
      |var cols = svg.append("g");
      |var hRulers = svg.append("g");
      |var vRulers = svg.append("g");
      |
      |var actions = [$actions]
      |
      |function updateCells(data) {
      |    var text = cells.selectAll("text")
      |        .data(data);
      |
      |    text.enter().append("text")
      |        .attr("class", "cell")
      |
      |    text.transition().text(function(d) {return d.value;})
      |        .attr("x", function(d) {return (d.col + 0.5) * cw + rowHeaderSize;} )
      |        .attr("y", function(d) {return d.row * ch + colHeaderSize - textOffsetY;} );
      |
      |    text.exit().remove();
      |}
      |
      |function updateRows(data) {
      |    var text = rows.selectAll("text")
      |        .data(data)
      |
      |    text.enter().append("text")
      |        .attr("class", "row")
      |
      |    text.attr("x", function(d) {return 0} )
      |        .attr("y", function(d) {return d.row * ch + colHeaderSize - textOffsetY;} )
      |        .text(function(d) {return d.name;})
      |
      |    text.exit().remove();
      |}
      |
      |function updateCols(data) {
      |    var padding = 4
      |    var text = cols.selectAll("text")
      |        .data(data)
      |
      |    text.enter().append("text")
      |        .attr("class", "col")
      |
      |    text.attr("x", function(d) {return (d.col + 0.5) * cw + rowHeaderSize;} )
      |        .attr("y", function(d) {return colHeaderSize - ch - padding;} )
      |        .text(function(d) {return d.name;})
      |        .attr("transform",function(d) { return "rotate(-45 " + ((d.col + 0.5) * cw + rowHeaderSize) + "," + (colHeaderSize - ch - padding) + ")" });
      |
      |    text.exit().remove();
      |}
      |
      |function updateHRulers(data) {
      |    var line = hRulers.selectAll("line")
      |        .data(data)
      |
      |    line.enter().append("line")
      |        .attr("class", "hruler")
      |
      |    line.transition().attr("x1", function(d) {return 0;} )
      |        .attr("y1", function(d) {return (d-1) * ch + colHeaderSize;} )
      |        .attr("x2", function(d) {return numCols * cw + rowHeaderSize;} )
      |        .attr("y2", function(d) {return (d-1) * ch + colHeaderSize;} )
      |
      |    line.exit().remove();
      |}
      |
      |function updateBoxes(data) {
      |
      |
      |    var rect = hRulers.selectAll("rect")
      |        .data(data)
      |
      |    rect.enter().append("rect")
      |        .attr("class", "box")
      |
      |    rect.transition()
      |        .attr("x", function(d) {return d.col * cw + rowHeaderSize + boxPadding;} )
      |        .attr("y", function(d) {return (d.row - 1) * ch + colHeaderSize + boxPadding;} )
      |        .attr("width", function(d) {return d.width * cw - 2 * boxPadding;} )
      |        .attr("height", function(d) {return d.height * ch - 2 * boxPadding;} )
      |
      |    rect.exit().remove();
      |}
      |
      |function updateAll(action) {
      |    updateCells(action.cells);
      |    updateRows(action.rows);
      |    updateCols(action.cols);
      |    updateHRulers(action.hRulers);
      |    updateBoxes(action.boxes);
      |}
      |
      |
      |var currentAction = 0;
      |updateAll(actions[currentAction]);
      |
      |
      |div.on("click", function() {
      |        if (currentAction < actions.length - 1)
      |           currentAction += 1;
      |        else
      |           currentAction = 0;
      |        updateAll(actions[currentAction]);
      |})
      |
      |//updateCells(cells1);
      |//updateRows(rowNames1);
      |//updateCols(colNames1);
      |//updateHRulers(hRulers1);
      |
      |//setTimeout(function() { updateCells(cells2); },2000);
      |
      |
      |
      |</script>
    """.stripMargin
    RawHTML(html)
  }
}
