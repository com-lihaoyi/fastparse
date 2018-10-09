package fasterparser

import scala.annotation.Annotation

object NoWhitespace {
  implicit object noWhitespaceImplicit extends (Parse[_] => Parse[Unit]){
    def apply(cfg: Parse[_]) = fasterparser.Parsing.Pass(cfg)

    override def toString() = "noWhitespaceImplicit"
  }

}

