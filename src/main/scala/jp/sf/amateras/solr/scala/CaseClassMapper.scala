package jp.sf.amateras.solr.scala

import scala.collection.JavaConversions._
import java.util.{Collection => JavaCollection, Set => JavaSet, List => JavaList}
import java.lang.reflect.{ParameterizedType, Type}
import org.joda.time._
import scala.Some

/**
 * Provides conversion methods for Map and case class.
 */
private[scala] object CaseClassMapper {

  val JavaCollection = classOf[JavaCollection[_]]
  val JavaList = classOf[JavaList[_]]
  val JavaSet = classOf[JavaSet[_]]
  val ScalaList = classOf[List[_]]
  val ScalaSet = classOf[Set[_]]
  val Option = classOf[Option[_]]
  val Void = classOf[Void]
  val JodaDateTime: Class[_] = try {
    classOf[DateTime]
  } catch {
    case e: LinkageError => Void
  }

  /**
   * Converts the given Map[String, Any] to the case class.
   *
   */
  def map2class[T](map: Map[String, Any])(implicit m: scala.reflect.Manifest[T]): T = {
    val clazz = m.erasure.asInstanceOf[Class[T]]

    val constructor = clazz.getConstructors()(0)
    val paramTypes = constructor.getParameterTypes
    val params = paramTypes.map(getDefaultValue(_).asInstanceOf[java.lang.Object])

    val instance = constructor.newInstance(params: _*).asInstanceOf[T]

    def convert(toType: Type)(value: Any): Any = {
      val (to: Class[_], subtype) = if (toType.isInstanceOf[ParameterizedType]) {
        val param = toType.asInstanceOf[ParameterizedType]
        (param.getRawType, param.getActualTypeArguments.head)
      } else {
        (toType.asInstanceOf[Class[_]], null)
      }
      if (value == null) {
        if (to == classOf[Option[_]]) None else null
      } else {
        val from = value.getClass
        if (to.isAssignableFrom(Option)) {
          Some(convert(subtype)(value))
        } else if (JavaCollection.isAssignableFrom(from)) {
          lazy val coll = value.asInstanceOf[JavaCollection[_]]
          if (to.isAssignableFrom(ScalaList)) {
            coll.toList.map(convert(subtype))
          } else if (to.isAssignableFrom(ScalaSet)) {
            coll.toSet.map(convert(subtype))
          } else if (to.isAssignableFrom(JavaList)) {
            val list: JavaList[_] = coll.toList.map(convert(subtype))
            list
          } else if (to.isAssignableFrom(JavaSet)) {
            val set: JavaSet[_] = coll.toSet.map(convert(subtype))
            set
          } else {
            throw new IllegalArgumentException("Unsupported target collection type " + to)
          }
        } else if (to.isAssignableFrom(JodaDateTime)) {
          new DateTime(value, DateTimeZone.UTC)
        } else {
          value
        }
      }
    }

    clazz.getDeclaredFields.foreach {
      field =>
        val value = map.get(field.getName).orNull
        field.setAccessible(true)
        field.set(instance, convert(field.getGenericType)(value))
    }

    instance
  }

  /**
   * Converts the case class to the Map[String, Any].
   *
   */
  def class2map(instance: Any): Map[String, Any] = {
    val fields = instance.getClass.getDeclaredFields
    fields.map {
      field =>
        field.setAccessible(true)
        val value = field.get(instance)

        value match {
          case Some(x) => (field.getName, x)
          case None => (field.getName, null)
          case _ => (field.getName, value)
        }
    }.toMap
  }

  def toMap(obj: Any): Map[String, Any] = {
    obj match {
      case null => Map[String, Any]()
      case map: Map[_, _] => map.asInstanceOf[Map[String, Any]]
      case x => class2map(x)
    }
  }

  def toMapArray(objs: Any*): Array[Map[String, Any]] = {
    objs.map {
      obj => toMap(obj)
    }.toArray
  }

  /**
   * Returns the default value for the given type.
   */
  private def getDefaultValue(clazz: Class[_]): Any = {
    if (clazz == classOf[Int] || clazz == java.lang.Integer.TYPE ||
      clazz == classOf[Short] || clazz == java.lang.Short.TYPE ||
      clazz == classOf[Byte] || clazz == java.lang.Byte.TYPE) {
      0
    } else if (clazz == classOf[Double] || clazz == java.lang.Double.TYPE) {
      0d
    } else if (clazz == classOf[Float] || clazz == java.lang.Float.TYPE) {
      0f
    } else if (clazz == classOf[Long] || clazz == java.lang.Long.TYPE) {
      0l
    } else if (clazz == classOf[Char] || clazz == java.lang.Character.TYPE) {
      '\0'
    } else if (clazz == classOf[Boolean] || clazz == java.lang.Boolean.TYPE) {
      false
    } else {
      null
    }
  }

}
