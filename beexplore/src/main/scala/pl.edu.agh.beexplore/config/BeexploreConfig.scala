package pl.edu.agh.beexplore.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.Signal

final case class BeexploreConfig(gridSize: Int,
                                 guiCellSize: Int,
                                 signalSuppressionFactor: Double,
                                 signalAttenuationFactor: Double,
                                 workersRoot: Int,
                                 shardingMod: Int,
                                 guiType: GuiType,
                                 isSupervisor: Boolean,
                                 signalSpeedRatio: Int,
                                 iterationsNumber: Long,

                                 beehiveX: Int,
                                 beehiveY: Int,
                                 beeSignalInitial: Signal,
                                 beeSignalAcquired: Signal,
                                 beeHungerThreshold: Int
                                ) extends XinukConfig {}
