package hubcat

import net.liftweb.json.{ JString, JNothing, JValue, JInt }

trait Jsonizing {  
  def jStringOrNone(opt: Option[String]): JValue = opt.map(JString(_)).getOrElse(JNothing)
  def jIntOrNone(opt: Option[Int]): JValue = opt.map(JInt(_)).getOrElse(JNothing)
}
