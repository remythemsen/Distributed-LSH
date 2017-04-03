package messages

trait Status
case object NotReady extends Status
case class InProgress(p:Int) extends Status
case object Ready extends Status
