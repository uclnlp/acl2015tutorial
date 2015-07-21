package uk.ac.ucl.cs.mr.acltutorial

import ml.wolfe.Vect
import org.sameersingh.htmlgen.Custom.Vectors
import org.sameersingh.htmlgen.{D3jsConverter, RawHTML, HTML}

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
        {Unparsed(htmls.head.source)}
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
    RawHTML(printer.format(html))
  }

  def points(data:(Seq[Vect],Seq[String])*) = {
    val vectors = for (((vectors,names),i) <- data.zipWithIndex; (v,n) <- vectors zip names) yield (i,n,v.toSeq)
    println("POINTS Output")
    println(vectors.length)
    println(vectors.mkString("\n"))
    D3jsConverter.convert(Vectors(vectors).norm)
  }


}
