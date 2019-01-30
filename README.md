### 为什么需要auto-mapper
在开发的过程中，一个相似的数据类经常会在多个层次中出现。
例如在DAO层有一个ProductRow类，用来存放product table的字段。
```
case class ProductRow(id: Long, seller: Long, name: String)
```
在业务逻辑层，有一个Product类，表示的也是一个product，但是seller的类型不是一个用户id，而是一个User类的object。
```
case class Product(id: Long, seller: User, name: String)
```

例如在DAO层，有一个UserRow类，里面包括了用户的各种credential
```
case class UserRow(id: Long, password: String, ...)
```
但是在别的抽象层次，并不需要用户的密码等信息，例如一个专门做序列化然后返回json给前端的User类。
```
case class User(id: Long, ...)
```

要在这些case class之间做转换，需要写大量的代码，特别是在写ontology特别多的应用的时候。user, product, address, payment, ...,每一个业务实体都要写一整套的转换代码
```
def productRowToProduct(row: ProductRow): Product: {
  Product(
    id = row.id
    seller = userRowToUser(dao.byId(row.seller)),
    name = row.name
    )
}
```

auto-mapper就是解决这个问题的，不需要每一个类写转换的代码，就可以用一行代码完成转换
```
// 召唤一个能把ProductRow映射成Product的instance
val p = Projection[ProductRow, Product]
val product = p.to(row)
```

### auto mapper可以做什么
- 不同顺序，A(i: Int, l: Long, //...其他基础类型) -> B(l: Long, i: Int, //...)
- Option可以开启/关闭，A(i: Option[Int]) -> B(i: Int)
- 类型不同，用户提供自定义转换方法
  ```
  implicit def vToW(v: V): W = ???
  A(fieldName: V) -> B(fieldName: W)
  ```
- A的字段比B多(隐藏部分字段), A(i: Int, hide: Long, ...) -> B(i: Int, ...)
- nested type自动转换，如果A可以转换成B，那么C可以转换成D
  ```
  case class C(a: A, ...)
  case class D(b: B, ...)
  ```

### relative works
[bfil](https://github.com/bfil/scala-automapper)的auto-mapper，这个是用反射获取class的各个field的名称和类型，然后用模板生成转换代码。   
[nevillelyh](https://github.com/nevillelyh/shapeless-datatype)的RecordMapper，使用shapeless做generic programming实现的。只有当需要转换的两个类字段的顺序和个数完全一样的时候，才能转换。主要参考了这个项目的实现，增加了对字段顺序不同，个数不同(隐藏部分字段)的支持。

### examples
在build.sbt中添加

#### 基本转换
```
case class A(i: Int, l: Long)
case class B(l: Long, i: Int)

val p = Projection[A, B]
val a = A(1, 1L)
val b = B(1L, 1)
val b_ = p.to(a)
b shouldEqual b_
```

#### 隐藏部分字段
```
case class A(i: Int, l: Long, hide: String)
case class B(l: Long, i: Int)

val p = Projection[A, B]
val a = A(1, 1L, "some secret")
val b = B(1L, 1)
val b_ = p.to(a)
b shouldEqual b_
```

#### 开启Option[A]转A功能(默认关闭)
```
case class A(i: Int, l: Option[Long])
case class B(l: Long, i: Int)

import Projection.UnsafeOptionExtractorImplicits._
val p = Projection[A, B]
val a = A(1, Some(1L))
val b = B(1L, 1)
val b_ = p.to(a)
b shouldEqual b_ 
```

#### 自定义转换方法
```
// 在数据库里面，images是用逗号分隔的方式存储的
case class A(images: String)
// 在业务层中，images是list of url
case class B(images: List[String])

// 用户自定义一个从String转换成List[String]的方法，注意控制implicit的scope
implicit def imagesSplit(s: String): List[String] = 
  images.split(",").toList
  
val a = A("1.png,2.png,3.jpg")
val b = B(List("1.png", "2.png", "3.jpg"))
val p = Projection[A, B]
val b_ = p.to(a)
b shouldEqual b_ 
```

#### nested type
```

```

### 如何实现的
case class可以转换成他的generic representation, HList。  

#### 基本思路
基本思路是利用shapeless的Generic，把一个case class转成HList，再从HList转成另一个case class
```
case class A(i: Int, s: String)
case class B(i: Int, s: String)

val a = A(1, "s")
val b = B(2, "s")

val genA = Generic[A]
val genB = Generic[B]
val hlistA = genA.to(a) // 1 :: "s" :: HNil
val b_ = genB.from(hlistA)  // B(2, "s")
```

现在可以写一个最基本的Projection[A, B]，当A和B的字段完全一样的时候可以转换
```
// 限制A和B的generic representation是一样的(都是L)
class Projection[A, B] {
  def to[L <: HList](a: A)(implicit
                                         genA: LabelledGeneric.Aux[A, L],
                                         genB: LabelledGeneric.Aux[B, L])
  : B = genB.from(genA.to(a))
}
```

如果要支持上面说的字段乱序和隐藏，需要额外定义一个MapRecord，MapRecord提供一个把A的HList转换成
B的HList的方法
```
trait MapRecord[LI <: HList, LO <: HList] {
  def apply(l: LI): LO
}
```

现在可以给Projection提供多一个MapRecord。表示A和B的generic representation不必完全一样，只需要
LA能够转换到LB就足够了。
```
class Projection[A, B] extends Serializable {
  def to[LA <: HList, LB <: HList](a: A)(implicit
                                         genA: LabelledGeneric.Aux[A, LA],
                                         genB: LabelledGeneric.Aux[B, LB],
                                         mr: MapRecord[LA, LB])
  : B = genB.from(mr(genA.to(a)))
}
```

#### 对HList进行修改
关于怎么写MapRecord，其实就是对输入的type LI进行一些type level的操作，使之变成type LO。

##### 乱序
shapeless提供了一个Selector，可以在HList里面获取对应的key value pair。
```
val a = A(1, "s")
val genA = LabelledGeneric[A]
val hlistA = genA.to(a)

val i = Witness('i)
val s = Witness('s)

val iSelector = Selector[genA.Repr, i.T]
val sSelector = Selector[genA.Repr, s.T]

val iValue = iSelector(hlistA)  // 1，key[i]对应的值
val sValue = sSelector(hlistA)  // "s", key[s]对应的值
```

可以利用Selector，循环LO中所有的field Key，在LI中select对应的key value pair
```
// 假设B有两个field，keys分别是[s, i]
// 两个keys各有一个对应的Selector
val iSelector = Selector[genA.Repr, i.T]
val sSelector = Selector[genA.Repr, s.T]
// 按照B中field的顺序对LI进行select
val hlistB = sSelector(hlistA) :: iSelector(hlistA) :: HNil
```

在这个例子中，成功把一个Int :: String :: HNil转换成了String :: Int :: HNil。现在可以利用
递归，让编译器自动推导所有的乱序转换
```
// 递归终点，SourceHList要转换成HNil，HNil里面什么field都没有，所以根本不需要select，直接
// 返回HNil就可以
implicit def hnilMapRecord[SourceHList <: HList]: MapRecord[SourceHList, HNil] = new MapRecord[SourceHList, HNil] {
    override def apply(l: SourceHList): HNil = HNil
  }

// 递归case，假设TargetHList的Tail可以转换(mrT)，并且可以在SourceHList中选出field K(select)
// 那么就可以推导出存在一个MapRecord[SourceHList, FieldType[K, V] :: TargetHListTail]
implicit def hconsMapRecordBase[K, V, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, V],
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]])
  : MapRecord[SourceHList, FieldType[K, V] :: TargetHListTail] = new MapRecord[SourceHList, FieldType[K, V] :: TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, V] :: TargetHListTail =
      field[K](select(l)) :: mrT.value(l)
  }
```

##### LO的field比LI的field少
通过上面Selector，自动解决了这个问题。因为是递归循环选LO的所有field。

##### LI中某个key对应的类型是V，但是LO中key对应的类型是W
V和W虽然不是一样的类型，但是只需要有一个从V到W的函数，就能完成转换。   
只需要在自动推导的时候增加一个implicit V => W
```
// 只是一个type alias，方便自己不用重复写很长的type
type MV[SourceHList <: HList, K, V, TargetHListTail <: HList] =
    MapRecord[SourceHList, FieldType[K, V] :: TargetHListTail]
    
implicit def hconsMapRecord1[K, V, W, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, V],
   f: V => W,
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]])
  : MV[SourceHList, K, W, TargetHListTail] = new MV[SourceHList, K, W, TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, W] :: TargetHListTail =
      field[K](f(select(l))) :: mrT.value(l)
  }
```

##### LI中某个key对应的类型是Option[V]，但是LO中key对应的类型是V
提供一个UnsafeOptionExtractorImplicits，当这个implicit在scope里的时候，可以把Option[V]变成V
```
object UnsafeOptionExtractorImplicits {
  implicit def apply[T]: UnsafeOptionExtractor[T] = new UnsafeOptionExtractor[T]
}
```
如果在LI中V，在LO中是Option[V]，只需要在select了V之后加上Option functor的pure方法即可。

##### nested type
递归的对nested type进行转换
```
implicit def hconsMapRecord0[K, V, W, VRepr <: HList, WRepr <: HList, SourceHList <: HList, TargetHListTail <: HList]
  (implicit
   select: Selector.Aux[SourceHList, K, V],
   genV: LabelledGeneric.Aux[V, VRepr],
   genW: LabelledGeneric.Aux[W, WRepr],
   mrH: Lazy[MapRecord[VRepr, WRepr]],
   mrT: Lazy[MapRecord[SourceHList, TargetHListTail]])
  : MV[SourceHList, K, W, TargetHListTail] = new MV[SourceHList, K, W, TargetHListTail] {
    override def apply(l: SourceHList): FieldType[K, W] :: TargetHListTail =
      field[K](genW.from(mrH.value(genV.to(select(l))))) :: mrT.value(l)
  }
```
