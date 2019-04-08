package autoMapper

import org.scalatest.{Matchers, WordSpec}

class ProjectionSpec extends WordSpec with Matchers {

  "" should {
    "交换field的顺序" in {
      case class A(i: Int, l: Long)
      case class B(l: Long, i: Int)

      val p = Projection[A, B]
      val a = A(1, 1L)
      val b = B(1L, 1)
      val b_ = p.to(a)
      b shouldEqual b_
    }
    "隐藏部分字段" in {
      case class A(i: Int, l: Long, hide: String)
      case class B(l: Long, i: Int)

      val p = Projection[A, B]
      val a = A(1, 1L, "some secret")
      val b = B(1L, 1)
      val b_ = p.to(a)
      b shouldEqual b_
    }
    "开启Option[A]转A功能(默认关闭)" in {
//      import autoMapper.UnsafeOptionExtractorImplicits._
      implicit val unsafeOptionExtractor = UnsafeOptionExtractorImplicits[Long]
      case class A(i: Int, l: Option[Long])
      case class B(l: Long, i: Int)

      val p = Projection[A, B]
      val a = A(1, Some(1L))
      val b = B(1L, 1)
      val b_ = p.to(a)
      b shouldEqual b_
    }
    "自定义转换方法" in {
      // 在数据库里面，images是用逗号分隔的方式存储的
      case class A(images: String)
      // 在业务层中，images是list of url
      case class B(images: List[String])

      // 用户自定义一个从String转换成List[String]的方法，注意控制implicit的scope
      implicit def imagesSplit(s: String): List[String] =
        s.split(",").toList

      val a = A("1.png,2.png,3.jpg")
      val b = B(List("1.png", "2.png", "3.jpg"))
      val p = Projection[A, B]
      val b_ = p.to(a)
      b shouldEqual b_
    }
    "nested type" in {
      case class A(i: Int, l: Long, c: C)
      case class B(l: Long, i: Int, c: C)
      case class C(s: String, i: Int)

      val a = A(1, 1L, C("c", 1))
      val b = B(1L, 1, C("c", 1))
      val p = Projection[A, B]
      val b_ = p.to(a)
      b shouldEqual b_
    }
  }
}