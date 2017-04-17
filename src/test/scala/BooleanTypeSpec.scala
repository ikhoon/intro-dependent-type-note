import org.scalatest.FunSuite

/**
  * Created by liam on 18/04/2017.
  */
class BooleanTypeSpec extends FunSuite {

  test("boolean or") {
    import TLImplicits._
    import shapeless._
    val Bool = "Boolean" :: Type
    val b = "b" :: Bool
    val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
    val tru :: fls :: HNil = BoolInd.intros
    val recBBB = BoolInd.rec(Bool ->: Bool)
    // recursive인것에 주목한다.
    // tru는 첫번재 항목을 가져오는 연산자.
    // fls는 해당 리스트의 두번째 항목을 가져오는 연산자이다.
    // true 다음은 false이고 false 다음은 true이다.
    // tru(fls) => flase가 나오고
    // fls(fls) => true가 나오게 된다.
    // fls(tru) => fls 가 나오게 된다.

    val or = recBBB(b :-> tru)(b :-> b)

    assert(or(tru)(tru).fansi == "true")
    assert(or(tru)(fls).fansi == "true")
    assert(or(fls)(tru) == tru)
    assert(or(fls)(fls) == fls)

    or(or(b)(or(b)(tru)))(or(b)(or(fls)(b)))
    // ((rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ (true : (Boolean : 𝒰 _0)))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))) (((rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ (true : (Boolean : 𝒰 _0)))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))) (b : (Boolean : 𝒰 _0)) : ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))) (((rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ (true : (Boolean : 𝒰 _0)))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))) (b : (Boolean : 𝒰 _0)) : ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))) (true : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)) : ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))) (((rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ (true : (Boolean : 𝒰 _0)))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))) (b : (Boolean : 𝒰 _0)) : ((Boolean : ?...

  }

  test("boolean xor") {

    import TLImplicits._
    import shapeless._
    val Bool = "Boolean" :: Type
    val b = "b" :: Bool
    val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
    val tru :: fls :: HNil = BoolInd.intros
    val recBB = BoolInd.rec(Bool)
    val not = recBB(fls)(tru)
    val recBBB = BoolInd.rec(Bool ->: Bool)
    val xor = recBBB(b :-> not(b))(b :-> b)

    assert(xor(tru)(tru).fansi == "false")
    assert(xor(tru)(fls).fansi == "true")
    assert(xor(fls)(tru) == tru)
    assert(xor(fls)(fls) == fls)
    xor(xor(b)(xor(not(b))(fls)))(xor(b)(xor(tru)(not(b))))
    // ((rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ ((rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(false : (Boolean : 𝒰 _0))(true : (Boolean : 𝒰 _0))) (b : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))) (((rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ ((rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(false : (Boolean : 𝒰 _0))(true : (Boolean : 𝒰 _0))) (b : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))) (b : (Boolean : 𝒰 _0)) : ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))) (((rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ ((rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(false : (Boolean : 𝒰 _0))(true : (Boolean : 𝒰 _0))) (b : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))) ((rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(false : (Bo...

  }

  test("boolean equal") {
    import TLImplicits._
    import shapeless._
    val Bool = "Boolean" :: Type
    val b = "b" :: Bool
    val BoolInd = ("true" ::: Bool) |: ("false" ::: Bool) =: Bool
    val tru :: fls :: HNil = BoolInd.intros
    val recBB = BoolInd.rec(Bool)
    val not = recBB(fls)(tru)
    val recBBB = BoolInd.rec(Bool ->: Bool)
    val isEqual = recBBB(b :-> b)(b :-> not(b))
    assert(isEqual(tru)(tru).fansi == "true")
    assert(isEqual(tru)(fls).fansi == "false")
    assert(isEqual(fls)(tru) == fls )
    assert(isEqual(fls)(fls) == tru)
    peisEqual(isEqual(not(b))(tru))(isEqual(not(b))(fls))
    // ((rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))((b : (Boolean : 𝒰 _0)) ↦ ((rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(false : (Boolean : 𝒰 _0))(true : (Boolean : 𝒰 _0))) (b : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)))) (((rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))((b : (Boolean : 𝒰 _0)) ↦ ((rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(false : (Boolean : 𝒰 _0))(true : (Boolean : 𝒰 _0))) (b : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)))) ((rec(Boolean : 𝒰 _0)(Boolean : 𝒰 _0)(false : (Boolean : 𝒰 _0))(true : (Boolean : 𝒰 _0))) (b : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)) : ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))) (true : (Boolean : 𝒰 _0)) : (Boolean : 𝒰 _0)) : ((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))) (((rec(Boolean : 𝒰 _0)((Boolean : 𝒰 _0) → (Boolean : 𝒰 _0))((b : (Boolean : 𝒰 _0)) ↦ (b : (Boolean : 𝒰 _0)))((b : (Boolea...

  }
}
