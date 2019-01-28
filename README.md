---
layout: post
title: "scala auto mapper"
date: "2019-01-28 10:49:38 +0800"
category: scala
author: linzihao
---

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
.......
