// Simple script to help testing of VL4S demonstrations

import uk.rwpenney.vl4s.demo.Demo

val tmpdir = java.nio.file.Paths.get("/tmp")

Seq("Trivial", "Waves", "Bessel", "GaussMix").foreach { m =>
  val tgt = tmpdir.resolve(s"vl4s-${m}.html").toString

  println(s"Generating ${tgt}")
  Demo.main(Seq("-m", m, "-f", "Webpage", "-o", tgt).toArray)
}
