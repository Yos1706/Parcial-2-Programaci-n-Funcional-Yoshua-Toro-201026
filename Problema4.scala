abstract class Servicio(
    val nombre: String,
    val costoBase: Double
) {
    def calcularCostoTotal(): Double

    def descripcion(): String = {
        s"Servicio: $nombre y su costo Base: $costoBase"
    }

    def +(otro: Servicio): Servicio
}

trait Promocion{
    val codigoPromo: String
    def aplicarPromocion(): String = s"Promoción es activada con el código: $codigoPromo"
}

class Invitacion(
    nombre: String,
    costoBase: Double,
    val numeroInvitados: Int,
    val costoPorInvitado: Double,
    val codigoPromo: String
) extends Servicio(nombre, costoBase) {
    override def calcularCostoTotal(): Double = {
        val total = costoBase + (numeroInvitados * costoPorInvitado)
        if codigoPromo == "FIESTAFIESTA" then total * 0.9 else total
    }

    override def +(otro: Servicio): Servicio = {
        new Invitacion(
            s"${this.nombre} + ${otro.nombre}",
            this.costoBase + otro.costoBase,
            this.numeroInvitados,
            this.costoPorInvitado,
            this.codigoPromo
        )
    }
}

object Invitacion {
    def aplicar(nombre: String, costoBase: Double, invitados: Int, costoInvitado: Double, codigoPromo: String): Invitacion = {
        require(invitados > 0, "El número de invitados debe ser mayor a 0 para crear tener servicio.")
        new Invitacion(nombre, costoBase, invitados, costoInvitado, codigoPromo)
    }
}

class Fotografia(
    nombre: String,
    costoBase: Double,
    val horas: Int,
    val horasExtras : Int,
) extends Servicio(nombre, costoBase) {
    override def calcularCostoTotal(): Double = {
        costoBase + (horas * horasExtras)
    }

    override def +(otro: Servicio): Servicio = {
        new Fotografia(
            s"${this.nombre} + ${otro.nombre}",
            this.costoBase + otro.costoBase,
            this.horas,
            this.horasExtras
        )
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        val servicioInvitacion = Invitacion.aplicar("Boda", 300, 100, 15, "FIESTAFIESTA")
        val servicioFoto = new Fotografia("Fotografía Profesional", 150, 5, 100)
    }
}

