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
  case class Matrix(cells:Seq[Cell], rowLabels:Seq[RowLabel], colLabels:Seq[ColLabel], hRulers:Seq[Int]) {
    override def toString = {
      s"""{"cells": [${cells.mkString(",")}],
         |"cols":[${colLabels.mkString(",")}],
         |"rows":[${rowLabels.mkString(",")}],
         |"hRulers":[${hRulers.mkString(",")}]}""".stripMargin
    }
  }

  case class Layout(cw:Int = 30, ch:Int = 30, rowHeaderSize:Int = 50, colHeaderSize:Int = 50, numCols:Int = 4, numRows:Int = 4)

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
      |
      |var width = numCols * cw + rowHeaderSize,
      |    height = numRows * ch + colHeaderSize,
      |    div = d3.select('#$id'),
      |    svg = div.append('svg')
      |        .attr('width', width)
      |        .attr('height', height);
      |
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
      |        .attr("x", function(d) {return d.col * cw + rowHeaderSize;} )
      |        .attr("y", function(d) {return d.row * ch + colHeaderSize;} );
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
      |        .attr("y", function(d) {return d.row * ch + colHeaderSize;} )
      |        .text(function(d) {return d.name;})
      |
      |    text.exit().remove();
      |}
      |
      |function updateCols(data) {
      |    var text = cols.selectAll("text")
      |        .data(data)
      |
      |    text.enter().append("text")
      |        .attr("class", "col")
      |
      |    text.attr("x", function(d) {return d.col * cw + rowHeaderSize;} )
      |        .attr("y", function(d) {return colHeaderSize - ch;} )
      |        .text(function(d) {return d.name;})
      |        .attr("transform",function(d) { return "rotate(-45 " + (d.col * cw + rowHeaderSize) + "," + (colHeaderSize - ch) + ")" });
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
      |        .attr("y1", function(d) {return d * ch + ch / 2.4 + colHeaderSize;} )
      |        .attr("x2", function(d) {return 3 * cw + rowHeaderSize;} )
      |        .attr("y2", function(d) {return d * ch + ch / 2.4 + colHeaderSize;} )
      |
      |    line.exit().remove();
      |}
      |
      |function updateAll(action) {
      |    updateCells(action.cells);
      |    updateRows(action.rows);
      |    updateCols(action.cols);
      |    updateHRulers(action.hRulers);
      |}
      |
      |var currentAction = 0;
      |updateAll(actions[currentAction]);
      |
      |
      |div.on("click", function() {
      |        currentAction += 1;
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
