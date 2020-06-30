// Original work Copyright 2009-2016 EPFL, Lausanne
// Modified work Copyright 2017 MPI-SWS, Saarbruecken, Germany

package daisy.lang

import scala.annotation.StaticAnnotation


import scala.language.implicitConversions
import scala.math.{ScalaNumericConversions, ScalaNumber}

@ignore
class ignore     extends StaticAnnotation

@ignore
object Real {
    @ignore
    implicit def double2real(d: Double): Real = Real(d)

    @ignore
    implicit def int2real(i: Int): Real = Real(i.toDouble)

    @ignore
    def sqrt(x: Real): Real = ???

    @ignore
    def sin(x: Real): Real = ???

    @ignore
    def cos(x: Real): Real = ???

    @ignore
    def tan(x: Real): Real = ???

    @ignore
    def asin(x: Real): Real = ???

    @ignore
    def acos(x: Real): Real = ???

    @ignore
    def atan(x: Real): Real = ???    

    @ignore
    def log(x: Real): Real = ???

    @ignore
    def exp(x: Real): Real = ???

    @ignore
    def pow(x: Real, y: Int): Real = ???

    @ignore
    def fma(x: Real, y: Real, z: Real): Real = ???
}

@ignore
case class Real private[daisy](v: Double) extends ScalaNumber with ScalaNumericConversions with Ordered[Real] {
    def unary_-(): Real = ???
    def +(other: Real): Real = ???
    def -(other: Real): Real = ???
    def *(other: Real): Real = ???
    def /(other: Real): Real = ???

    // Uncertainty on this value
    def +/-(x: Real): Boolean = ???

    def compare(other: Real): Int = ???

    def underlying(): AnyRef = this
    def isWhole(): Boolean = ???
    def doubleValue(): Double = ???
    def floatValue(): Float = ???
    def longValue(): Long = ???
    def intValue(): Int = ???
}