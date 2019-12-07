package pl.edu.agh.beexplore.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}

final case class BeexploreConfig(gridSize: Int,
                                 guiCellSize: Int,
                                 signalSuppressionFactor: Double,
                                 signalAttenuationFactor: Double,
                                 workersRoot: Int,
                                 shardingMod: Int,
                                 guiType: GuiType,
                                 isSupervisor: Boolean,
                                 signalSpeedRatio: Int,
                                 iterationsNumber: Long
                                ) extends XinukConfig {}
