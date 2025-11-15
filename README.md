# Лабораторная работа №2: Структуры данных

**Выполнил:** Кравченко Дмитрий Олегович  
**Группа:** P3319

---

## 1. Требования к разработанному ПО

### 1.1 Цель работы

Освоение построения пользовательских типов данных, полиморфизма, рекурсивных алгоритмов и средств тестирования (unit testing, property-based testing), а также разделения интерфейса и особенностей реализации.

### 1.2 Выбранная структура данных

**Open Addressing Hash Map (Хеш-таблица с открытой адресацией)** с линейным пробированием.

### 1.3 Функциональные требования

Реализация должна обеспечивать:

1. **Основные операции:**

   - `make_empty : unit -> 'v t` - создание пустого словаря
   - `insert : key -> 'v -> 'v t -> 'v t` - добавление элемента
   - `remove : key -> 'v t -> 'v t` - удаление элемента
   - `find : key -> 'v t -> 'v option` - поиск элемента
   - `mem : key -> 'v t -> bool` - проверка наличия ключа

2. **Функции высшего порядка:**

   - `map : 'v t -> ('v -> 'r) -> 'r t` - отображение (изменение типа значений)
   - `filter : 'v t -> ('v -> bool) -> 'v t` - фильтрация элементов
   - `fold_left : 'acc -> ('acc -> key -> 'v -> 'acc) -> 'v t -> 'acc` - левая свёртка
   - `fold_right : 'v t -> (key -> 'v -> 'acc -> 'acc) -> 'acc -> 'acc` - правая свёртка

3. **Моноидальные свойства:**

   - `concat : 'v t -> 'v t -> 'v t` - бинарная ассоциативная операция
   - Нейтральный элемент: `make_empty ()`
   - Свойства: ассоциативность, левая и правая идентичность

4. **Вспомогательные функции:**
   - `equal : ('v -> 'v -> bool) -> 'v t -> 'v t -> bool` - эффективное сравнение
   - `size : 'v t -> int` - количество элементов
   - `to_list : 'v t -> (key * 'v) list` - преобразование в список
   - `of_list : (key * 'v) list -> 'v t` - создание из списка

### 1.4 Нефункциональные требования

1. **Иммутабельность:** все операции возвращают новые структуры данных, не изменяя исходные
2. **Полиморфизм:** структура полиморфна по типу значений (`'v`)
3. **Производительность:**
   - Средняя сложность вставки/поиска: O(1)
   - Автоматическое увеличение размера при load factor > 0.7
4. **Функциональный стиль:** использование рекурсии вместо циклов
5. **Модульность:** чёткое разделение интерфейса (`.mli`) и реализации (`.ml`)

---

## 2. Ключевые элементы реализации

### 2.1 Определение типов данных

```ocaml
type key = int

type 'v slot =
  | None        (* Пустой слот *)
  | Tombstone   (* Удалённый элемент *)
  | Pair of (key * 'v)  (* Ключ и значение *)

(* Основная структура данных *)
type 'v t = {
  size: int;              (* Количество элементов *)
  buckets: 'v slot array; (* Массив слотов *)
}
```

**Комментарий:** Используется открытая адресация с tombstone-маркерами для поддержки удаления элементов без нарушения цепочек пробирования.

### 2.2 Хеширование и пробирование

```ocaml
(* Хеш-функция: модуль по размеру таблицы *)
let hash key cap =
  (abs key) mod cap

(* Рекурсивный поиск свободного слота с линейным пробированием *)
let rec find_slot buckets key cap idx first_tombstone =
  if idx >= cap then
    match first_tombstone with
    | Some i -> i  (* Используем первый найденный tombstone *)
    | None -> 0
  else
    match buckets.(idx) with
    | None ->
        (match first_tombstone with
         | Some i -> i  (* Предпочитаем tombstone для вставки *)
         | None -> idx)
    | Tombstone ->
        let ft = match first_tombstone with
          | None -> Some idx
          | Some _ -> first_tombstone
        in
        find_slot buckets key cap ((idx + 1) mod cap) ft
    | Pair (k, _) ->
        if k = key then idx
        else find_slot buckets key cap ((idx + 1) mod cap) first_tombstone
```

**Комментарий:** Линейное зондирование с учётом tombstone-маркеров для оптимизации повторного использования удалённых слотов.

### 2.3 Иммутабельная вставка

```ocaml
let insert k v d =
  (* Увеличиваем размер при превышении load factor *)
  let d = if load_factor d > 0.7 then resize d else d in

  let cap = capacity d in
  let h = hash k cap in
  let idx = find_slot d.buckets k cap h None in

  (* Копируем массив для иммутабельности *)
  let new_buckets = Array.copy d.buckets in
  match new_buckets.(idx) with
  | Pair (key, _) when key = k ->
      new_buckets.(idx) <- Pair (k, v);
      { d with buckets = new_buckets }
  | _ ->
      new_buckets.(idx) <- Pair (k, v);
      { size = d.size + 1; buckets = new_buckets }
```

**Комментарий:** Использование `Array.copy` гарантирует иммутабельность операции вставки.

### 2.4 Рекурсивное отображение (map)

```ocaml
let map d f =
  (* Полиморфная рекурсивная функция с явными типовыми аннотациями *)
  let rec map_slots : type v r. v slot array -> (v -> r) -> int -> r slot array -> r slot array =
    fun old_buckets func idx acc ->
      if idx >= Array.length old_buckets then acc
      else
        let new_slot = match old_buckets.(idx) with
          | Pair (k, v) -> Pair (k, func v)
          | Tombstone -> Tombstone
          | None -> None
        in
        acc.(idx) <- new_slot;
        map_slots old_buckets func (idx + 1) acc
  in
  let new_buckets = Array.make (Array.length d.buckets) None in
  let result_buckets = map_slots d.buckets f 0 new_buckets in
  { size = d.size; buckets = result_buckets }
```

**Комментарий:** Явные типовые аннотации `type v r.` необходимы для полиморфизма по типу значений при изменении типа с `'v` на `'r`.

### 2.5 Рекурсивная фильтрация

```ocaml
let filter d pred =
  let rec filter_slots idx acc =
    if idx >= Array.length d.buckets then acc
    else
      let new_acc = match d.buckets.(idx) with
        | Pair (k, v) when pred v -> insert k v acc  (* Вставляем если предикат истинен *)
        | _ -> acc
      in
      filter_slots (idx + 1) new_acc
  in
  filter_slots 0 (make_empty ())
```

**Комментарий:** Фильтрация создаёт новый словарь, добавляя только элементы, удовлетворяющие предикату.

### 2.6 Свёртки (левая и правая)

```ocaml
let fold_left acc f d =
  let rec fold_slots idx accumulator =
    if idx >= Array.length d.buckets then accumulator
    else
      let new_acc = match d.buckets.(idx) with
        | Pair (k, v) -> f accumulator k v
        | _ -> accumulator
      in
      fold_slots (idx + 1) new_acc
  in
  fold_slots 0 acc

let fold_right d f acc =
  let rec fold_slots idx =
    if idx < 0 then acc
    else
      match d.buckets.(idx) with
      | Pair (k, v) -> f k v (fold_slots (idx - 1))
      | _ -> fold_slots (idx - 1)
  in
  fold_slots (Array.length d.buckets - 1)
```

**Комментарий:** Свёртки реализованы через хвостовую и нехвостовую рекурсию соответственно.

### 2.7 Эффективное сравнение

```ocaml
let equal eq d1 d2 =
  if d1.size <> d2.size then false
  else
    (* Проходим по первому словарю и проверяем наличие в втором *)
    fold_left true (fun acc k v1 ->
      if not acc then false
      else
        match find k d2 with
        | Option.Some v2 -> eq v1 v2
        | Option.None -> false
    ) d1
```

**Комментарий:** Сравнение выполняется за O(n), не требуя приведения к спискам и сортировки.

---

## 3. Тестирование

### 3.1 Unit-тесты (19 тестов)

Использована библиотека **Alcotest**.

#### Категории тестов:

**Базовые операции:**

- `empty dict` - проверка пустого словаря
- `insert single element` - вставка одного элемента
- `insert multiple elements` - вставка нескольких элементов
- `insert overwrites existing key` - перезапись существующего ключа
- `remove element` - удаление элемента
- `find non-existent key` - поиск несуществующего ключа

**Функции высшего порядка:**

- `map function` - применение отображения
- `filter function` - фильтрация элементов
- `fold_left` - левая свёртка
- `fold_right` - правая свёртка

**Моноидальные свойства:**

- `monoid left identity` - левая идентичность: `empty + d = d`
- `monoid right identity` - правая идентичность: `d + empty = d`
- `monoid associativity` - ассоциативность: `(a + b) + c = a + (b + c)`

**Вспомогательные функции:**

- `concat two dicts` - объединение двух словарей
- `concat with override` - объединение с перезаписью
- `to_list` - преобразование в список
- `of_list` - создание из списка
- `equal dicts` - сравнение одинаковых словарей
- `not equal dicts` - сравнение разных словарей

### 3.2 Property-based тесты (10 тестов)

Использована библиотека **QCheck** с интеграцией **qcheck-alcotest**.

#### Тестируемые свойства:

**Инварианты операций:**

1. `insert increases size` - вставка увеличивает или сохраняет размер
2. `insert then find` - вставка и поиск возвращают то же значение
3. `remove decreases size` - удаление уменьшает или сохраняет размер

**Моноидальные свойства (обязательно для моноида):** 4. `monoid left identity` - `empty + d = d` для всех `d` 5. `monoid right identity` - `d + empty = d` для всех `d` 6. `monoid associativity` - `(a + b) + c = a + (b + c)` для всех `a, b, c`

**Свойства функций высшего порядка:** 7. `to_list . of_list preserves keys` - композиция сохраняет ключи 8. `map preserves size` - отображение сохраняет размер 9. `filter decreases size` - фильтрация уменьшает или сохраняет размер

**Свойства сравнения:** 10. `equal is reflexive` - `d = d` для всех `d` (рефлексивность)

### 3.3 Результаты тестирования

```
Testing `OA_Dict'.
This run has ID `3EIMEVC4'.

  [OK]          Unit Tests                    0   empty dict
  [OK]          Unit Tests                    1   insert single element
  [OK]          Unit Tests                    2   insert multiple elements
  [OK]          Unit Tests                    3   insert overwrites existing key
  [OK]          Unit Tests                    4   remove element
  [OK]          Unit Tests                    5   find non-existent key
  [OK]          Unit Tests                    6   map function
  [OK]          Unit Tests                    7   filter function
  [OK]          Unit Tests                    8   fold_left
  [OK]          Unit Tests                    9   fold_right
  [OK]          Unit Tests                   10   concat two dicts
  [OK]          Unit Tests                   11   concat with override
  [OK]          Unit Tests                   12   to_list
  [OK]          Unit Tests                   13   of_list
  [OK]          Unit Tests                   14   equal dicts
  [OK]          Unit Tests                   15   not equal dicts
  [OK]          Unit Tests                   16   monoid left identity
  [OK]          Unit Tests                   17   monoid right identity
  [OK]          Unit Tests                   18   monoid associativity
  [OK]          Property-Based Tests          0   insert increases size
  [OK]          Property-Based Tests          1   insert then find
  [OK]          Property-Based Tests          2   remove decreases size
  [OK]          Property-Based Tests          3   monoid left identity
  [OK]          Property-Based Tests          4   monoid right identity
  [OK]          Property-Based Tests          5   monoid associativity
  [OK]          Property-Based Tests          6   to_list . of_list
  [OK]          Property-Based Tests          7   map preserves size
  [OK]          Property-Based Tests          8   filter decreases size
  [OK]          Property-Based Tests          9   equal is reflexive

Test Successful in 0.260s. 29 tests run.
```

---

## 4. Выводы

### 4.1 Об использованных приёмах программирования

**Практические навыки:**

- Работа с системой сборки Dune
- Использование библиотек тестирования Alcotest и QCheck
- Проектирование модульного интерфейса в OCaml
- Применение продвинутых возможностей системы типов OCaml

**Возможные улучшения:**

- Оптимизация копирования массивов (COW - Copy-On-Write)
- Использование персистентных структур данных (например, HAMTs)
- Добавление функций для работы с итераторами
- Расширение property-based тестов (например, тестирование сериализации/десериализации)

Данная работа показала, что функциональный подход не только теоретически элегантен, но и практически применим для реализации эффективных структур данных с гарантиями корректности через строгую типизацию и исчерпывающее тестирование.
