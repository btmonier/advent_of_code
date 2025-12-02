plugins {
    kotlin("jvm") version "2.2.20"
    application
}

application {
    val dayNum = (project.findProperty("day") as String?) ?: "01"
    mainClass.set("Day${dayNum}Kt")
}

group = "org.btmonier"
version = "0.1"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(21)
}