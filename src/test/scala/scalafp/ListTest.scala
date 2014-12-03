package scalafp

import java.util.NoSuchElementException

import org.scalatest.FunSuite

/**
 * Created by f.casasus on 06/11/14.
 */
class ListTest extends FunSuite{
  test ("Sum works correctly") {
    val l = List(1, 2 ,3)
    assert(List.sum(l) == 6)
  }

  test ("tail works correctly") {
    val l = List(1, 2, 3)
    assert(List.tail(l) == List(2, 3))
  }

  test ("drop works correctly") {
    val l = List(1, 2, 3)
    assert(List.drop(l, 2) == List(3))
    intercept[NoSuchElementException](List.drop(l, 4))
    intercept[NoSuchElementException](List.drop(l, -1))
    intercept[NoSuchElementException](List.drop(Nil, 1))
  }

  test("reverse works as expected") {
    val l = List (1, 2 ,3)
    assert(List.reverse(l) == List(3, 2, 1))
  }

  test("Append word as expected") {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5)

    assert(List.append(l1, l2) == List(1, 2, 3, 4, 5))
  }

  test("Flatten list works correctly") {
    val l = List(List(1, 2), List(3), Nil:List[Int], List(4, 5))
    assert(List.flatten(l) == List(1, 2, 3, 4 , 5))
  }

  test("map should return a new list") {
    val l = List (1, 2 ,3)
    assert(List.map(l){x => x * x} == List(1, 4, 9))
  }

  test("flatmap must return a flatten list") {
    val l = List(1, 2, 3)
    assert(List.flatMap(l)(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  test("filter test") {
    val l = List(1, 10, 20, 30, 40)
    val f1 = List.filter(l) (x => x < 21)
    assert(f1 == List(1, 10, 20))
    assert(f1 == List.filterWithFP(l)(x => x < 21))
  }

  test("ZipWith works with generic functions") {
    assert(List.zipWith(List(1, 2, 3), List(4, 5, 6, 9))((x,y) => x + y) == List(5, 7, 9))
  }

  test("Contains works as expected") {
    assert(List.contains(List(1,2), List(8,1,3,1,4,1,2,5)) == true)
    assert(List.contains(Nil:List[Int], List(8,1,3,1,4,1,2,5)) == true)
    assert(List.contains(List(1,20), List(8,1,3,1,4,1,2,5)) == false)
  }
}
