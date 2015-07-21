package uk.ac.ucl.cs.mr.acltutorial

import org.sameersingh.htmlgen.{RawHTML, HTML}

import scala.xml.{Unparsed, XML, PrettyPrinter}

/**
 * @author riedelcastro
 */
object Renderer {

  def fragments(htmls: Seq[HTML], startIndex: Int = 0) = {
    val printer = new PrettyPrinter(0, 2)

    require(htmls.nonEmpty)
    val html = <div style="position:relative;">
      <div class="fragment fade-out" data-fragment-index={startIndex.toString}>
        {htmls.head.source}
      </div>{for (i <- 1 until htmls.length) yield
      <div class="fragment current-visible" data-fragment-index={(i - 1 + startIndex).toString}
           style="position: absolute;top: 0; left: 0; width: 100%; height:100%; z-index: 10;">
        {Unparsed(htmls(i).source)}
      </div>}
    </div>
    RawHTML(printer.format(html))
  }

  def floatLeft(htmls: Seq[HTML]) = {
    val printer = new PrettyPrinter(0, 2)
    val html =
      <div>
        {for (html <- htmls) yield  <div style="float:left;">{Unparsed(html.source)}</div>}
      </div>
    println("FLOATLEFT Output")
    println(printer.format(html))
    RawHTML(printer.format(html))
  }

}
