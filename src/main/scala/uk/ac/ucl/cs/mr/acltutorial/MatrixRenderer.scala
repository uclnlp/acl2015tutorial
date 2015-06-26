package uk.ac.ucl.cs.mr.acltutorial

import java.util.UUID

import org.sameersingh.htmlgen.{RawHTML, HTML}

/**
* @author riedelcastro
*/
object MatrixRenderer {
  def render():HTML = {
    val id = "matrix" + UUID.randomUUID().toString
    val html = s"""
      |<div id = "$id" class="matrix">
      |</div>
      |<script>
      |var width = 300,
      |    height = 300,
      |    div = d3.select('#$id'),
      |    svg = div.append('svg')
      |        .attr('width', width)
      |        .attr('height', height),
      |    rw = 95,
      |    rh = 95;
      |
      |var cw = 100, ch = 100;
      |
      |var cells = svg.append("g");
      |var rows = svg.append("g");
      |var cols = svg.append("g");
      |var hRulers = svg.append("g");
      |var vRulers = svg.append("g");
      |
      |// data are animation steps
      |var cells1 = [
      |    {"row": 1, "col": 1, "value":1.2},
      |    {"row": 1, "col": 2, "value":1},
      |    {"row": 2, "col": 1, "value":1}
      |    ];
      |
      |var cells2 = [
      |    {"row": 1, "col": 1, "value":1},
      |    {"row": 2, "col": 2, "value":0}
      |    ];
      |
      |var rowNames1 = [
      |    {"row":1, "name":"row1"},
      |    {"row":2, "name":"row2"}
      |];
      |
      |var colNames1 = [
      |    {"col":1, "name":"col1"},
      |    {"col":2, "name":"col2"}
      |];
      |
      |var hRulers1 = [1,2];
      |var hRulers2 = [2];
      |
      |var actions = [
      |    {"cells": cells1, "cols": colNames1, "rows": rowNames1, "hRulers": hRulers1},
      |    {"cells": cells2, "cols": colNames1, "rows": rowNames1, "hRulers": hRulers2}
      |];
      |
      |
      |function updateCells(data) {
      |    var text = cells.selectAll("text")
      |        .data(data);
      |
      |    text.enter().append("text")
      |        .attr("class", "cell")
      |
      |    text.text(function(d) {return d.value;})
      |        .attr("x", function(d) {return d.col * cw;} )
      |        .attr("y", function(d) {return d.row * ch;} );
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
      |        .attr("y", function(d) {return d.row * ch;} )
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
      |    text.attr("x", function(d) {return d.col * cw;} )
      |        .attr("y", function(d) {return 50;} )
      |        .text(function(d) {return d.name;})
      |        .attr("transform",function(d) { return "rotate(-45 " + d.col * cw + ",50)" });
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
      |    line.attr("x1", function(d) {return 0;} )
      |        .attr("y1", function(d) {return d * ch + ch / 2;} )
      |        .attr("x2", function(d) {return 3 * cw;} )
      |        .attr("y2", function(d) {return d * ch + ch / 2;} )
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
