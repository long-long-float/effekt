package effekt

import effekt.context.Context
import effekt.util.messages.FatalPhaseError

/**
 * A Phase is *not* itself a task! Phases might _use_ tasks, but
 * per se do not perform any caching and can be re-run on an
 * input to produce a new output. They do not track dependencies.
 */
trait Phase[In, Out] {
  def run(input: In)(implicit C: Context): Option[Out]

  def apply(input: In)(implicit C: Context): Option[Out] = try {
    run(input)
  } catch {
    case FatalPhaseError(msg) =>
      C.error(msg)
      None
  }

  /**
   * Helper method to find the currently implicit context
   */
  def Context(implicit ctx: Context): Context = ctx
}
