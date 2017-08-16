# fs-stats: Aggregation of statistic with file system

Библиотека для сохранения статистики в виде KV-пар.

Стаистики представлены закрытым типом `Kv`.
По сути он хранит набор значений по ключам.
Сам тип Kv закрыт, мы можем только создать новое пустое значение и сохранять значения в YAML-файл:

~~~haskell
newKv :: IO Kv

writeKv :: FilePath -> Kv -> IO ()
~~~

### Ссылки Ref

Пользователь может добавлять и обновлять значения через интерфейс ссылок:

Мы можем создавать и удалять поля из KV-значения:

~~~haskell
newRef :: IsPrimVal a => Kv -> Text -> a -> IO (Ref a)
newRef kv fieldName initValue

rmRef :: Kv -> Text -> IO ()
rmRef kv fieldName
~~~

Мы можем записывтаь, читать и обновлять значения по ссылкам:

~~~haskell
writeRef  :: IsPrimVal a => Ref a -> a -> IO ()
readRef   :: IsPrimVal a => Ref a -> IO a
modifyRef :: IsPrimVal a => Ref a -> (a -> a) -> IO ()
~~~

Все обновления происходят атомарно через STM под капотом.

Обратим внимание на зависимость IsPrimVal. Этот класс содержит примитивные типы.
Определены инстансы для `Text`, `String`, `Int, `Float`, `Double`, `Bool`, `Scientific`.
Также мы можем определить свои инстансы (см модуль `Data.Stat.Fs.PrimVal`).

## Сохранение метрик

Метрики хранятся в виде простого YAML-файла.
Для сохранения можно воспользоваться функцией `writeKv`:

~~~haskell
writeKv :: FilePath -> Kv -> IO ()
~~~

Пример:

Запустим `stack ghci` в проекте `fs-stats`:

~~~haskell
kv    <- newKv
count <- newRef kv "count" (0 :: Int)
flag  <- newRef kv "flag" True
evt   <- newRef kv "evt" ("on" :: String)
writeKv "test-stat.yaml" kv

modifyRef count (+1)
writeRef evt "off"
writeKv "test-stat.yaml" kv
~~~

Отметим, что файл сохраняется не напрямую. а сначала создаётся временный файл,
а затем этот файл переименовывается в файл с заданным именем.

### Сохранение метрик в бесконечном цикле

Часто необходимо периодически обновлять значения метрик.
Для этого сценария есть функция:

~~~haskell
writeKvLoop :: NominalDiffTime -> FilePath -> Kv -> IO ()
writeKvLoop timeDelta file kv
~~~

Она периодически сохраняет статистику в файл.

Пример:

Продолжим пример из предыдущего раздела:

~~~haskell
writeKvLoop 5 "test-stat.yaml" kv
~~~

Далее мы можем обновить статистику и убедиться, что значения обновились и в файле:

~~~haskell
writeRef count 7
~~~
