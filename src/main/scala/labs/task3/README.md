# 3. Параллельная обработка последовательностей
   
## Общее условие:
+ 3.1. Реализуйте параллельный вариант filter (не обязательно ленивый) с помощью future.
  Параллельная обработка должна производиться блоками по заданному числу элементов. Размер
  блоков следует вычислять вручную, без использования готовых функций, таких как partition (для
  разделения последовательности следует использовать take и drop). Продемонстрируйте прирост
  производительности в сравнении с обычным фильтром.
+ 3.2. Реализуйте ленивый параллельный filter, который должен работать в том числе с бесконечными
  потоками. Продемонстрируйте прирост производительности в сравнении с обычным фильтром.