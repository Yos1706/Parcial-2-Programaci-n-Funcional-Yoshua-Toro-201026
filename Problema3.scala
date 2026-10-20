import scala.annotation.tailrec

object Problema3 {

    def procesarRango(
        desde: Int,
        hasta: Int,
        transformar: Int => Int,
        combinar: (Int, Int) => Int,
        valorInicial: Int
    ): Int = {
        if desde > hasta then valorInicial
        else {
            val transformado = transformar(desde)
            val resultadosSiguienteRango = procesarRango(desde + 1, hasta, transformar, combinar, valorInicial)
            combinar(transformado, resultadosSiguienteRango)
        }
    }

    def buscar(
        desde: Int,
        hasta: Int,
        condicion: Int => Boolean
    ): Int = {
        @annotation.tailrec
        def iteracion(actual: Int): Int = {
            if actual > hasta then -1
            else if condicion(actual) then actual
            else iteracion(actual + 1)
        }
        iteracion(desde)
    }

    def contarDivisores(a: Int): Int = {
        @annotation.tailrec
        def contar(divisor: Int, acc: Int): Int = {
            if divisor > a then acc
            else if a % divisor == 0 then contar(divisor + 1, acc + 1)
            else contar(divisor + 1, acc)
        }
        contar(1, 0)
    }

    def esPrimo(a: Int): Boolean = {
        if a <= 1 then false
        else {
            @annotation.tailrec
            def verificar(divisor: Int): Boolean = {
                if divisor * divisor > a then true
                else if a % divisor == 0 then false
                else verificar(divisor + 1)
            }
            verificar(2)
        }
    }

    def main(args: Array[String]): Unit = {
        def maximosDivisoresEnRango(desde: Int, hasta: Int): Int = {
            procesarRango(desde, hasta, contarDivisores, (a, b) => if a > b then a else b, 0)
        }
        println(maximosDivisoresEnRango(5, 10))
        println(maximosDivisoresEnRango(22, 26))
        def primoMasBajo(desde: Int, hasta: Int): Int = {
            buscar(desde, hasta, esPrimo)
        }
        println(primoMasBajo(6,11))
        println(primoMasBajo(24,28))
    }
}