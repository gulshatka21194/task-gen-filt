# Модуль взаимодействия с Excel.
## Содержание
- [Подключение модуля к проекту](#подключение-модуля-к-проекту)
- [Взаимодействие с Excel](#взаимодействие-с-excel)
  - [Запуск через панель](#запуск-через-панель)
  - [Запуск через кнопку на существующей таблице](#запуск-через-кнопку-на-существующей-таблице)
  
Необходимое требование - установленный Excel.

Поддерживаются следующие диаграммы:
  - гистограмма,
  - гистограмма с накоплением,
  - нормированная гистограмма с накоплением,
  - пузырьковая диаграмма,
  - график,
  - кольцевая диаграмма
  - вторичная линейчатая диаграмма
  - лепестковая диаграмма
 
# Подключение модуля к проекту
- Добавить проект `Excel Module` как подпроект

- Пример содержимого файла config:
```
[general]
pvss_path = "C:/Siemens/Automation/WinCC_OA/3.18"
# Путь к подпроекту
proj_path = "D:/Projects/ExcelModule"

# Путь к исполняемому проекту должен быть указан последним 
proj_path = "D:/Projects/Project"

proj_version = "3.18"

langs = "en_US.utf8"
langs = "ru_RU.utf8"
lang = "ru_RU.utf8"
useNGA = 1
```

# Взаимодействие с Excel.
## Запуск через панель

- Запустить менеджер "Интерфейс пользователя" с параметрами -p vision/EM_main.xml.

![Главное окно взаимодействия с Excel](/images/EM_main.jpg "Главное окно взаимодействия с Excel")

- Нажать на кнопку "Импорт/Экспорт". Далее выбрать файл Excel.

![Окно импорта/экспорта](/images/EM_impexp.jpg "Окно импорта/экспорта")

- Есть опции перезаписи названия столбцов и форматирования ячееек.
- При загрузке файла Excel на пустую панель необходимо выбрать опцию - "Перезаписать названия столбцов".
- Форматирование ячеек - по желанию.
- Далее нажать на кнопку загрузить.

![Выбор файла и опций](/images/EM_impexp_choice.jpg "Выбор файла и опций")

- Максимальное количество объектов(диаграмма-таблица) на панели - 4.

![Добавление объектов на панель](/images/EM_main_objects.jpg "Добавление объектов на панель")

## Запуск через кнопку на существующей таблице.

- Нажать на кнопку.

![Таблица с данными](/images/EM_object_single.jpg "Таблица с данными")

- Выбрать файл и нажать на кнопку сохранить.

![Выбор файла и опций](/images/EM_object_single_choice.jpg "Выбор файла и опций")

- Если сохраняемый файл открыт - появится окно с соответствующим сообщением. 

![Окно с сообщением занятости файла](/images/EM_object_single_usage.jpg "Окно с сообщением занятости файла")

![Сохранение файла](/images/EM_object_single_save.jpg "Сохранение файла")

- Результат с форматированием.

![Данные в Excel с форматированием](/images/EM_object_single_format.jpg "Данные в Excel с форматированием")

- Результат без форматирования.

![Данные в Excel без форматирования](/images/EM_object_single_no_format.jpg "Данные в Excel без форматирования")
