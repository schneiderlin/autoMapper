import shapeless._
import shapeless.labelled.{field, FieldType}
import shapeless.ops.record.Selector

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait MapRecord[LI <: HList, LO <: HList] {
  def apply(l: LI): LO
}

trait LowPriorityMapRecordBase {
  type MV1[SourceHList <: HList, K, V, TargetHListTail <: HList] =
    MapRecord[SourceHList, FieldType[K, V] :: TargetHListTail]

  /**
    * 最基本的类型，V和W是一样的类型
    */
  implicit def hconsMapRecordBase[K, V, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, V],
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]])
  : MV1[SourceHList, K, V, TargetHListTail] = new MV1[SourceHList, K, V, TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, V] :: TargetHListTail =
      field[K](select(l)) :: mrT.value(l)
  }
}

trait LowPriorityMapRecord1 extends LowPriorityMapRecordBase {
  /**
    * V和W不是同一个类型，但是有implicit转换
    */
  implicit def hconsMapRecord1[K, V, W, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, V],
   f: V => W,
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]])
  : MV1[SourceHList, K, W, TargetHListTail] = new MV1[SourceHList, K, W, TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, W] :: TargetHListTail =
      field[K](f(select(l))) :: mrT.value(l)
  }
}

trait LowPriorityMapRecordOption5 extends LowPriorityMapRecord1 {
  /**
    * Key对应的是一个Option[V]，并且V和W不是同一个类型
    * 把V提取出来，然后再用V => W
    */
  implicit def hconsMapRecordOption5[K, V, W, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, Option[V]],
   f: V => W,
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]],
   extractor: UnsafeOptionExtractor[V])
  : MV1[SourceHList, K, W, TargetHListTail] = new MV1[SourceHList, K, W, TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, W] :: TargetHListTail =
      field[K](f(extractor.extract(select(l)))) :: mrT.value(l)
  }
}

trait LowPriorityMapRecordOption4 extends LowPriorityMapRecordOption5 {
  /**
    * W在Option里面
    * 用V => W，然后Some()
    */
  implicit def hconsMapRecordOption4[K, V, W, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, V],
   f: V => W,
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]])
  : MV1[SourceHList, K, Option[W], TargetHListTail] = new MV1[SourceHList, K, Option[W], TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, Option[W]] :: TargetHListTail =
      field[K](Some(f(select(l)))) :: mrT.value(l)
  }
}

trait LowPriorityMapRecordOption3 extends LowPriorityMapRecordOption4 {
  /**
    * V和W在Option里面
    * 直接v.map(f)得到Option[W]
    */
  implicit def hconsMapRecordOption3[K, V, W, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, Option[V]],
   f: V => W,
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]])
  : MV1[SourceHList, K, Option[W], TargetHListTail] = new MV1[SourceHList, K, Option[W], TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, Option[W]] :: TargetHListTail =
      field[K](select(l).map(f)) :: mrT.value(l)
  }
}

trait LowPriorityMapRecordIterable1 extends LowPriorityMapRecordOption3 {
  /**
    * V和W都在Iterable里面
    * 用map(f)把V全部变成W
    */
  implicit def hconsMapRecordIterable1[K, V, W, SourceHList <: HList, TargetHListTail <: HList, S[_] <: Iterable[_]]
  (implicit
   select: Selector.Aux[SourceHList, K, S[V]],
   f: V => W,
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]],
   cbf: CanBuildFrom[_, W, S[W]])
  : MV1[SourceHList, K, S[W], TargetHListTail] = new MV1[SourceHList, K, S[W], TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, S[W]] :: TargetHListTail = {
      val b = cbf()
      b ++= select(l).asInstanceOf[Iterable[V]].map(f)
      field[K](b.result()) :: mrT.value(l)
    }
  }
}

trait LowPriorityMapRecord0 extends LowPriorityMapRecordIterable1 {
  /**
    * V和W都是nested type，这两个type都可以转换成HList，并且HList之间可以map
    */
  implicit def hconsMapRecord0[K, V, W, VRepr <: HList, WRepr <: HList, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, V],
   genV: LabelledGeneric.Aux[V, VRepr],
   genW: LabelledGeneric.Aux[W, WRepr],
   mrH: Lazy[MapRecord[VRepr, WRepr]],
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]])
  : MV1[SourceHList, K, W, TargetHListTail] = new MV1[SourceHList, K, W, TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, W] :: TargetHListTail =
      field[K](genW.from(mrH.value(genV.to(select(l))))) :: mrT.value(l)
  }
}

trait LowPriorityMapRecordOption2 extends LowPriorityMapRecord0 {
  /**
    * V和W都是nested type，这两个type都可以转换成HList，并且HList之间可以map
    * V包在了Option里面，用extractor提取
    */
  implicit def hconsMapRecordOption2[K, V, W, VRepr <: HList, WRepr <: HList, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, Option[V]],
   genV: LabelledGeneric.Aux[V, VRepr],
   genW: LabelledGeneric.Aux[W, WRepr],
   mrH: Lazy[MapRecord[VRepr, WRepr]],
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]],
   extractor: UnsafeOptionExtractor[V])
  : MV1[SourceHList, K, W, TargetHListTail] = new MV1[SourceHList, K, W, TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, W] :: TargetHListTail =
      field[K](genW.from(mrH.value(genV.to(extractor.extract(select(l)))))) :: mrT.value(l)
  }
}

trait LowPriorityMapRecordOption1 extends LowPriorityMapRecordOption2 {
  /**
    * V和W都是nested type，这两个type都可以转换成HList，并且HList之间可以map
    * W包在了Option里面，最后在W外面包上一个Some()
    */
  implicit def hconsMapRecordOption1[K, V, W, VRepr <: HList, WRepr <: HList, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, V],
   genV: LabelledGeneric.Aux[V, VRepr],
   genW: LabelledGeneric.Aux[W, WRepr],
   mrH: Lazy[MapRecord[VRepr, WRepr]],
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]])
  : MV1[SourceHList, K, Option[W], TargetHListTail] = new MV1[SourceHList, K, Option[W], TargetHListTail] {
    override def apply(l: SourceHList): ::[FieldType[K, Option[W]], TargetHListTail] =
      field[K](Some(genW.from(mrH.value(genV.to(select(l)))))) :: mrT.value(l)
  }
}

trait LowPriorityMapRecordOption0 extends LowPriorityMapRecordOption1 {
  /**
    * V和W都是nested type，这两个type都可以转换成HList，并且HList之间可以map
    * V和W都包在Option里面，用Option.map
    */
  implicit def hconsMapRecordOption0[K, V, W, VRepr <: HList, WRepr <: HList, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, Option[V]],
   genV: LabelledGeneric.Aux[V, VRepr],
   genW: LabelledGeneric.Aux[W, WRepr],
   mrH: Lazy[MapRecord[VRepr, WRepr]],
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]])
  : MV1[SourceHList, K, Option[W], TargetHListTail] = new MV1[SourceHList, K, Option[W], TargetHListTail] {
    override def apply(l: SourceHList): ::[FieldType[K, Option[W]], TargetHListTail] =
      field[K](select(l).map(v => genW.from(mrH.value(genV.to(v))))) :: mrT.value(l)
  }
}

trait LowPriorityMapRecordIterable0 extends LowPriorityMapRecordOption0 {
  /**
    * V和W都是nested type，这两个type都可以转换成HList，并且HList之间可以map
    * V和W都包在了Iterable里面，用Iterable.map
    */
  implicit def hconsMapRecordIterable0[K, V, W, VRepr <: HList, WRepr <: HList, SourceHList <: HList, TargetHListTail <: HList, S[_] <: Iterable[_]]
  (implicit
   select: Selector.Aux[SourceHList, K, S[V]],
   genV: LabelledGeneric.Aux[V, VRepr],
   genW: LabelledGeneric.Aux[W, WRepr],
   mrH: Lazy[MapRecord[VRepr, WRepr]],
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]],
   cbf: CanBuildFrom[_, W, S[W]])
  : MV1[SourceHList, K, S[W], TargetHListTail] = new MV1[SourceHList, K, S[W], TargetHListTail] {
    override def apply(l: SourceHList): ::[FieldType[K, S[W]], TargetHListTail] = {
      val b = cbf()
      b ++= select(l).asInstanceOf[Iterable[V]].map(v => genW.from(mrH.value(genV.to(v))))
      field[K](b.result()) :: mrT.value(l)
    }
  }
}

object MapRecord extends LowPriorityMapRecordIterable0 {
  //  implicit val hnilMapRecord: MapRecord[HNil, HNil] = new MapRecord[HNil, HNil] {
  //    override def apply(l: HNil): HNil = l
  //  }

  implicit def hnilMapRecord[SourceHList <: HList]: MapRecord[SourceHList, HNil] = new MapRecord[SourceHList, HNil] {
    override def apply(l: SourceHList): HNil = HNil
  }
}

class Bijection[A, B] extends Serializable {
  def to[LA <: HList, LB <: HList](a: A)(implicit
                                         genA: LabelledGeneric.Aux[A, LA],
                                         genB: LabelledGeneric.Aux[B, LB],
                                         mr: MapRecord[LA, LB])
  : B = genB.from(mr(genA.to(a)))

  def from[LB <: HList, LA <: HList](b: B)(implicit
                                           genB: LabelledGeneric.Aux[B, LB],
                                           genA: LabelledGeneric.Aux[A, LA],
                                           mr: MapRecord[LB, LA])
  : A = genA.from(mr(genB.to(b)))
}

object Bijection {
  def apply[A, B]: Bijection[A, B] = new Bijection[A, B]()
}

class Projection[A, B] extends Serializable {
  def to[LA <: HList, LB <: HList](a: A)(implicit
                                         genA: LabelledGeneric.Aux[A, LA],
                                         genB: LabelledGeneric.Aux[B, LB],
                                         mr: MapRecord[LA, LB])
  : B = genB.from(mr(genA.to(a)))
}

object Projection {
  def apply[A, B]: Projection[A, B] = new Projection[A, B]()
}

class UnsafeOptionExtractor[T] {
  def extract(opt: Option[T]): T = opt.get
}

object UnsafeOptionExtractorImplicits {
  implicit def apply[T]: UnsafeOptionExtractor[T] = new UnsafeOptionExtractor[T]
}
