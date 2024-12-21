package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.ChangeSet

import scala.collection.mutable

trait ValidWithCachePhasePlugin[Input, Output] extends ValidPhasePlugin[Input, Output] {
  def cacheKey: String
  def getCache(cacheMap: scala.collection.mutable.Map[String, Any]): Output = cacheMap(cacheKey).asInstanceOf[Output]
  def runWithCache(input: Input, oldRoot: Output, changeSet: ChangeSet)(implicit flix: Flix, errors: mutable.ListBuffer[CompilationMessage]): Output = {
    throw new NotImplementedError("run method (with cache) must be implemented")
  }
}
