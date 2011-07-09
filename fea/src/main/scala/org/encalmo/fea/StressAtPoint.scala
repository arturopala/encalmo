package org.encalmo.fea

/** Stress at element point, outside nodes */
case class StressAtPoint(
        /** Point coordinates */
        coordinates:Vector,
        /** Stress */
        stress:NodeStress
) {

}