# fs-stats: Aggregation of statistic with file system

Библиотека для сохранения статистики в виде KV-пар.

Статистика это набор KV-пар:

~~~haskell
newtype Kv = Kv { unKv :: [Metric] }

data Metric = Metric
  { metricName :: Text
  , metricData :: PrimVal
  }
~~~

Примитивное значение (`PrimVal`) может быть строкой, числом и булевым значением.
Определны инстансы для `IsString`, `Num`, `Fractional`, `Boolean`.

Так метрику можно записать:

~~~haskell
{-# Language OverloadedStrings #-}
import Data.Boolean

stat = Kv [Metric "cpu" 10, Metric "roots" 25, Metric "active" true]
~~~

## Сохранение метрик

Метрики хранятся в виде простого YAML-файла.
Для сохранения можно воспользоваться функцией `writeStat`:

~~~haskell
writeStat :: FilePath -> Kv -> IO ()
~~~

Пример:

Запустим `stack ghci` в проекте `fs-stats`:

~~~haskell
 > writeStat "stat.yaml" $ Kv [Metric "cpu" 10, Metric "roots" 25, Metric "active" true]
~~~

Отметим, что файл сохраняется не напрямую. а сначала создаётся временный файл с
тем же именем, но `.tmp` на конце, а затем этот файл переименовывается в файл с заданным именем.

### Сохранение метрик в бесконечном цикле

Часто необходимо периодически обновлять значения метрик.
Для этого сценария есть функция:

~~~haskell
writeStatLoop :: NominalDiffTime -> FilePath -> TVar Kv -> IO ()
writeStatLoop timeDelta file tvStat
~~~

Она периодически сохраняет статистику из `TVar` переменной в файл.

Пример:

Запустим `stack ghci` в проекте `fs-stats`:

~~~haskell
tv <- newTVarIO (Kv [Metric "count" 0])
writeStatLoop 5 "stat.yaml" tv
~~~

Далее мы можем вручную обновить статистику и убедиться, что значения обновились и в файле:

~~~haskell
import Control.Monad.STM

atomically $ writeTVar tv (Kv [Metric "count": 1])
~~~
