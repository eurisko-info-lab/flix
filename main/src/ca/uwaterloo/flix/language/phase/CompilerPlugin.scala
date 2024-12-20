package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix

trait CompilerPlugin[Input, Output] {
  def name: String

  def convertInputType(in: Any): Input = in.asInstanceOf[Input]

  def run(input: Input)(implicit flix: Flix): Output
}
