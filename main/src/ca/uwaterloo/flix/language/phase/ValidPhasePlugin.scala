package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage

import scala.collection.mutable

trait ValidPhasePlugin[Input, Output] {
  def name: String

  def convertInputType(in: Any): Input = in.asInstanceOf[Input]

  def run(input: Input)(implicit flix: Flix, errors: mutable.ListBuffer[CompilationMessage]): Output = {
    throw new NotImplementedError("run method must be implemented")
  }
}
