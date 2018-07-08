# Пакеты для манипуляций с данными
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(purrr)
# Пакет для визуализации
library(ggplot2)
# Пакет для рейтингов
suppressPackageStartupMessages(library(comperank))

theme_set(theme_bw())

# Не должно понадобиться. Просто на всякий случай.
set.seed(20180703)


# Модель Эло --------------------------------------------------------------
#' @details Данная функция векторизована по всем своим аргументам. Использование
#' `...` критично для возможности передачи других аргументов в будущем.
#' 
#' @return Вероятность того, что игрок 1 (с рейтингом `rating1`) выиграет матч
#'   против игрока 2 (рейтинг `rating2`). Разница рейтингов напрямую влияет на
#'   результат.
elo_win_prob <- function(rating1, rating2, ksi = 400, ...) {
  norm_rating_diff <- (rating2 - rating1) / ksi
  
  1 / (1 + 10^norm_rating_diff)
}

#' @return Рейтинговая функция для модели Эло, которую можно передать в
#'   `comperank::add_iterative_ratings()`.
elo_fun_gen <- function(K, ksi = 400) {
  function(rating1, score1, rating2, score2) {
    comperank::elo(rating1, score1, rating2, score2, K = K, ksi = ksi)[1, ]
  }
}


# Модель ЭлоБета ----------------------------------------------------------
#' @details Данная функция векторизована по всем своим аргументам.
#' 
#' @return Вероятность того, что игрок 1 (с рейтингом `rating1`) выиграет матч
#'   против игрока 2 (рейтинг `rating2`). Матч играется до `frames_to_win`
#'   победных фреймов. Разница рейтингов напрямую влияет на вероятность победы
#'   в одном фрейме.
elobeta_win_prob <- function(rating1, rating2, frames_to_win, ksi = 400, ...) {
  prob_frame <- elo_win_prob(rating1 = rating1, rating2 = rating2, ksi = ksi)
  
  # Вероятность того, что первый игрок выиграет `frames_to_win` фреймов раньше
    # второго опираясь на вероятность первого игрока выиграть один фрейм
    # (`prob_frame`). Фреймы считаются независимыми.
  pbeta(prob_frame, frames_to_win, frames_to_win)
}

#' @return Результат матча в терминах победы первого игрока: 1 если он/она
#'   выиграл(а), 0.5 в случае ничьи и 0 если он/она проиграл(а).
get_match_result <- function(score1, score2) {
  # В снукере ничьи (обычно) не бывает, но это учитывает общий случай.
  near_score <- dplyr::near(score1, score2)
  
  dplyr::if_else(near_score, 0.5, as.numeric(score1 > score2))
}

#' @return Рейтинговая функция для модели ЭлоБета, которую можно передать в
#'   `comperank::add_iterative_ratings()`.
elobeta_fun_gen <- function(K, ksi = 400) {
  function(rating1, score1, rating2, score2) {
    prob_win <- elobeta_win_prob(
      rating1 = rating1, rating2 = rating2,
      frames_to_win = pmax(score1, score2), ksi = ksi
    )
    
    match_result <- get_match_result(score1, score2)
    delta <- K * (match_result - prob_win)
    
    c(rating1 + delta, rating2 - delta)
  }
}


# Данные эксперимента -----------------------------------------------------
# Функция для разделения наблюдений по типам "train", "validation" и "test"
split_cases <- function(n, props = c(0.5, 0.25, 0.25)) {
  breaks <- n * cumsum(head(props, -1)) / sum(props)
  id_vec <- findInterval(seq_len(n), breaks, left.open = TRUE) + 1
  
  c("train", "validation", "test")[id_vec]
}

pro_players <- snooker_players %>% filter(status == "pro")

# Матчи только между профессионалами
pro_matches_all <- snooker_matches %>%
  # Используем только реально состоявшиеся матчи
  filter(!walkover1, !walkover2) %>%
  # Оставляем только матчи между профессионалами
  semi_join(y = pro_players, by = c(player1Id = "id")) %>%
  semi_join(y = pro_players, by = c(player2Id = "id")) %>%
  # Добавляем столбец 'season'
  left_join(
    y = snooker_events %>% select(id, season), by = c(eventId = "id")
  ) %>%
  # Обеспечиваем упорядоченность по времени окончания матча
  arrange(endDate) %>%
  # Подготавливаем к формату widecr
  transmute(
    game = seq_len(n()),
    player1 = player1Id, score1, player2 = player2Id, score2,
    matchId = id, endDate, eventId, season,
    # Вычисляем тип матча ("train", "validation" или "test") в пропорции
      # 50/25/25
    matchType = split_cases(n())
  ) %>%
  # Конвертируем в формат widecr
  as_widecr()

# Матчи только между профессионалами в непригласительных турнирах (убираются, в
  # основном, турниры Championship League).
pro_matches_off <- pro_matches_all %>%
  anti_join(
    y = snooker_events %>% filter(type == "Invitational"),
    by = c(eventId = "id")
  )

# Функция для подтверждение разбиения
get_split <- . %>% count(matchType) %>% mutate(share = n / sum(n))

# Это должно давать разбиение 50/25/25 (train/validation/test)
pro_matches_all %>% get_split()

# Это даёт другое разбиение, потому что пригласительные турниры не распределены
  # равномерно в течение сезона. Однако, при таком подходе матчи разбиты на
  # основании тех же разделителей __по времени__, что и в `pro_matches_all`. Это
  # гарантирует, что матчи с одним типом представляют одинаковые __периоды во
  # времени__.
pro_matches_off %>% get_split()

# Сетка для коэффициента K
k_grid <- 1:100


# Функции эксперимента ----------------------------------------------------
#' @param matches Объект класса `longcr` или `widecr` со столбцом `matchType`
#'   (тип матча для эксперимента: "train", "validation" или "test").
#' @param test_type Тип матчей для вычисления качества модели. Для корректности
#'   эксперимента все матчи этого типа должны были проводиться позже всех других
#'   ("разогревочных") матчей. Это означает, что у них должны быть бОльшие
#'   значения столбца `game`.
#' @param k_vec Вектор коэффициентов K для вычисления качества модели.
#' @param rate_fun_gen Функция, которая при передаче коэффициента K возвращает
#'   рейтинговую функцию для передачи в `comperank::add_iterative_ratings()`.
#' @param get_win_prob Функция для вычисления вероятности победы на основании
#'   рейтингов игроков (`rating1`, `rating2`) и количества фреймов, необходимого
#'   для победы в матче (`frames_to_win`). __Замечание__: она должна быть
#'   векторизована по всем своим аргументам.
#' @param initial_ratings Начальные рейтинги в формате для
#'   `comperank::add_iterative_ratings()`.
#' 
#' @details Данная функция вычисляет:
#' - Историю итеративных рейтингов после упорядочивания `matches` по возрастанию
#' столбца `game`.
#' - Для матчей с типом `test_type`:
#'     - Вероятность победы игрока 1.
#'     - Результат матча в терминах победы первого игрока: 1 если он/она
#'     выиграл(а), 0.5 в случае ничьи и 0 если он/она проиграл(а).
#' - Качество в виде RMSE: квадратный корень из средней квадратичной ошибки, где
#' "ошибка" - разность между прогнозной вероятностью и результатом матча.
#' 
#' @return Tibble со столбцами 'k' для коэффициента K и 'goodness' для
#'   величины качества RMSE.
compute_goodness <- function(matches, test_type, k_vec, rate_fun_gen,
                             get_win_prob, initial_ratings = 0) {
  cat("\n")
  map_dfr(k_vec, function(cur_k) {
    # Отслеживание хода выполнения
    cat(cur_k, " ")
    matches %>%
      arrange(game) %>%
      add_iterative_ratings(
        rate_fun = rate_fun_gen(cur_k), initial_ratings = initial_ratings
      ) %>%
      left_join(y = matches %>% select(game, matchType), by = "game") %>%
      filter(matchType %in% test_type) %>%
      mutate(
        # Количество фреймов для победы в матче
        framesToWin = pmax(score1, score2),
        # Вероятность победы игрока 1 в матче до `framesToWin` побед
        winProb = get_win_prob(
          rating1 = rating1Before, rating2 = rating2Before,
          frames_to_win = framesToWin
        ),
        result = get_match_result(score1, score2),
        squareError = (result - winProb)^2
      ) %>%
      summarise(goodness = sqrt(mean(squareError)))
  }) %>%
    mutate(k = k_vec) %>%
    select(k, goodness)
}

#' Обёртка для `compute_goodness()` для использования с матрицей эксперимента
compute_goodness_wrap <- function(matches_name, test_type, k_vec,
                                  rate_fun_gen_name, win_prob_fun_name,
                                  initial_ratings = 0) {
  matches_tbl <- get(matches_name)
  rate_fun_gen <- get(rate_fun_gen_name)
  get_win_prob <- get(win_prob_fun_name)
  
  compute_goodness(
    matches_tbl, test_type, k_vec, rate_fun_gen, get_win_prob, initial_ratings
  )
}

#' Функция для осуществления эксперимента
#' 
#' @param test_type Вектор значений `test_type` (тип теста) для
#'   `compute_goodness()`.
#' @param rating_type Имена рейтинговых моделей (типы рейтинга).
#' @param data_type Суффиксы типов данных.
#' @param k_vec,initial_ratings Величины для `compute_goodnes()`.
#' 
#' @details Данная функция генерирует матрицу эксперимента и вычисляет несколько
#' значений качества моделей для разных комбинаций типов рейтинга и данных. Для
#' того, чтобы она работала, в глобальном окружении необходимо наличие
#' переменных по следующими комбинациями имён:
#' - "pro_matches_" + `<типы теста>` + `<типы данных>` для результатов матчей.
#' - `<типы рейтинга>` + "_fun_gen" для генераторов рейтинговых функций.
#' - `<типы рейтинга>` + "_win_prob" для функций, вычисляющий вероятность
#' победы.
#' 
#' @return Tibble со следующими столбцами:
#' - __testType__ <chr> : Идентификатор типа теста.
#' - __ratingType__ <chr> : Идентификатор типа рейтинга.
#' - __dataType__ <chr> : Идентификатор типа данных.
#' - __k__ <dbl/int> : Значение коэффициента K.
#' - __goodness__ <dbl> : Значение качества модели.
do_experiment <- function(test_type = c("validation", "test"),
                          rating_type = c("elo", "elobeta"),
                          data_type = c("all", "off"),
                          k_vec = k_grid,
                          initial_ratings = 0) {
  crossing(
    testType = test_type, ratingType = rating_type, dataType = data_type
  ) %>%
    mutate(
      dataName = paste0("pro_matches_", testType, "_", dataType),
      kVec = rep(list(k_vec), n()),
      rateFunGenName = paste0(ratingType, "_fun_gen"),
      winProbFunName = paste0(ratingType, "_win_prob"),
      initialRatings = rep(list(initial_ratings), n()),
      experimentData = pmap(
        list(dataName, testType, kVec,
             rateFunGenName, winProbFunName, initialRatings),
        compute_goodness_wrap
      )
    ) %>%
    unnest(experimentData) %>%
    select(testType, ratingType, dataType, k, goodness)
}


# Проведение эксперимента -------------------------------------------------
pro_matches_validation_all <- pro_matches_all %>% filter(matchType != "test")
pro_matches_validation_off <- pro_matches_off %>% filter(matchType != "test")
pro_matches_test_all <- pro_matches_all
pro_matches_test_off <- pro_matches_off

# Выполнение занимает существенное время
experiment_tbl <- do_experiment()

if (!dir.exists("articles")) {
  dir.create("articles")
}

saveRDS(
  experiment_tbl,
  file.path( "articles", "001_elo-and-elobeta-models-in-snooker_results.rds")
)


# Изучение результатов эксперимента ---------------------------------------
plot_data <- experiment_tbl %>%
  unite(group, ratingType, dataType) %>%
  mutate(
    testType = recode(
      testType, validation = "Валидационный", test = "Тестовый"
    ),
    groupName = recode(
      group, elo_all = "Эло, все матчи", elo_off = "Эло, офиц. матчи",
      elobeta_all = "ЭлоБета, все матчи",
      elobeta_off = "ЭлоБета, офиц. матчи"
    ),
    # Фиксация предпочтительного порядка
    groupName = factor(groupName, levels = unique(groupName))
  )

compute_optimal_k <- . %>% group_by(testType, groupName) %>%
  slice(which.min(goodness)) %>%
  ungroup()
compute_k_labels <- . %>% compute_optimal_k() %>%
  mutate(label = paste0("K = ", k)) %>%
  group_by(groupName) %>%
  # Если оптимальное K в рамках одной панели находится справа от своей пары,
    # её метке необходимо небольшое смещение вправо. Если слева - полное и
    # небольшое смещение влево.
  mutate(hjust = - (k == max(k)) * 1.1 + 1.05) %>%
  ungroup()

plot_experiment_results <- function(results_tbl) {
  ggplot(results_tbl) +
    geom_hline(
      yintercept = 0.5, colour = "#AA5555", size = 0.5, linetype = "dotted"
    ) +
    geom_line(aes(k, goodness, colour = testType)) +
    geom_vline(
      data = compute_optimal_k,
      mapping = aes(xintercept = k, colour = testType),
      linetype = "dashed", show.legend = FALSE
    ) +
    geom_text(
      data = compute_k_labels,
      mapping = aes(k, Inf, label = label, hjust = hjust),
      vjust = 1.2
    ) +
    facet_wrap(~ groupName) +
    scale_colour_manual(
      values = c(`Валидационный` = "#377EB8", `Тестовый` = "#FF7F00"),
      guide = guide_legend(title = "Эксперимент", override.aes = list(size = 4))
    ) +
    labs(
      x = "Коэффициент K", y = "Качество модели (RMSE)",
      title = "Лучшие значения качества моделей Эло и ЭлоБета почти равны",
      subtitle = paste0(
        'Использование официальных матчей (без пригласительных турниров) даёт ',
        'более устойчивые результаты.\n',
        'Оптимальные значения K из тестового эксперимента (с более длительным ',
        '"разогревом") меньше, чем из валидационного.'
      )
    ) +
    theme(title = element_text(size = 13), strip.text = element_text(size = 12))
}

plot_experiment_results(plot_data)


# Изучение рейтингов ЭлоБета в официальных матчах -------------------------
# Вспомогательная функция
gather_to_longcr <- function(tbl) {
  bind_rows(
    tbl %>% select(-matches("2")) %>% rename_all(funs(gsub("1", "", .))),
    tbl %>% select(-matches("1")) %>% rename_all(funs(gsub("2", "", .)))
  ) %>%
    arrange(game)
}

# Извлечение лучшего значения коэффициента K
best_k <- experiment_tbl %>%
  filter(testType == "test", ratingType == "elobeta", dataType == "off") %>%
  slice(which.min(goodness)) %>%
  pull(k)

  #!!! Округляет к "красивому" числу, т.к. это не сильно влияет на качество !!!
best_k <- round(best_k / 5) * 5

# Вычисление рейтингов на момент окончания данных
elobeta_ratings <- rate_iterative(
  pro_matches_test_off, elobeta_fun_gen(best_k), initial_ratings = 0
) %>%
  rename(ratingEloBeta = rating_iterative) %>%
  arrange(desc(ratingEloBeta)) %>%
  left_join(
    y = snooker_players %>% select(id, playerName = name), by = c(player = "id")
  ) %>%
  mutate(rankEloBeta = order(ratingEloBeta, decreasing = TRUE)) %>%
  select(player, playerName, ratingEloBeta, rankEloBeta)

elobeta_top16 <- elobeta_ratings %>%
  filter(rankEloBeta <= 16) %>%
  mutate(
    rankChr = formatC(rankEloBeta, width = 2, format = "d", flag = "0"),
    ratingEloBeta = round(ratingEloBeta, 1)
  )
  
official_ratings <- tibble(
  player = c(
         5,      1,    237,      17,     12,     16,    224,     30,
        68,    154,     97,      39,     85,      2,    202,   1260
  ),
  rankOff = c(
         2,      3,      4,       1,      5,      7,      6,     13,
        16,     10,      8,       9,     26,     17,     12,     23
  ),
  ratingOff = c(
    905750, 878750, 751525, 1315275, 660250, 543225, 590525, 324587,
    303862, 356125, 453875,  416250, 180862, 291025, 332450, 215125
  )
)


# Эволюция рейтингов ЭлоБета ----------------------------------------------
# Вспомогательные данные
seasons_break <- ISOdatetime(2017, 5, 2, 0, 0, 0, tz = "UTC")

# Вычисление эволюции рейтингов
elobeta_history <- pro_matches_test_off %>%
  add_iterative_ratings(elobeta_fun_gen(best_k), initial_ratings = 0) %>%
  gather_to_longcr() %>%
  left_join(y = pro_matches_test_off %>% select(game, endDate), by = "game")

# Генерирование графика
plot_all_elobeta_history <- function(history_tbl) {
  history_tbl %>%
    mutate(isTop16 = player %in% elobeta_top16$player) %>%
    ggplot(aes(endDate, ratingAfter, group = player)) +
      geom_step(data = . %>% filter(!isTop16), colour = "#C2DF9A") +
      geom_step(data = . %>% filter(isTop16), colour = "#22A01C") +
      geom_hline(yintercept = 0, colour = "#AAAAAA") +
      geom_vline(
        xintercept = seasons_break, linetype = "dotted",
        colour = "#E41A1C", size = 1
      ) +
      geom_text(
        x = seasons_break, y = Inf, label = "Конец 2016/17",
        colour = "#E41A1C", hjust = 1.05, vjust = 1.2
      ) +
      scale_x_datetime(date_labels = "%Y-%m") +
      labs(
        x = NULL, y = "Рейтинг ЭлоБета",
        title = paste0(
          "Большая часть текущего топ-16 определилась в конце сезона 2016/17"
        ),
        subtitle = paste0(
          "Победа в турнире хорошо заметна как существенный рост без падения в",
          " конце."
        )
      ) +
      theme(title = element_text(size = 13))
}

plot_all_elobeta_history(elobeta_history)


# Эволюция топ-16 по рейтингу ЭлоБета -------------------------------------
# Вычисление данных графика
top16_rating_evolution <- elobeta_history %>%
  # Функция `inner_join` позволяет оставить только игроков из `elobeta_top16`
  inner_join(y = elobeta_top16 %>% select(-ratingEloBeta), by = "player") %>%
  # Оставить матчи только из сезона 2017/18
  semi_join(
    y = pro_matches_test_off %>% filter(season == 2017), by = "game"
  ) %>%
  mutate(playerLabel = paste(rankChr, playerName))

# Генерирование графика
plot_top16_elobeta_history <- function(elobeta_history) {
  ggplot(elobeta_history) +
    geom_step(aes(endDate, ratingAfter, group = player), colour = "#22A01C") +
    geom_hline(yintercept = 0, colour = "#AAAAAA") +
    geom_rug(
      data = elobeta_top16,
      mapping = aes(y = ratingEloBeta), sides = "r"
    ) +
    facet_wrap(~ playerLabel, nrow = 4, ncol = 4) +
    scale_x_datetime(date_labels = "%Y-%m") +
    labs(
      x = NULL, y = "Рейтинг ЭлоБета",
      title = "Эволюция рейтинга ЭлоБета для топ-16 (на конец сезона 2017/18)",
      subtitle = paste0(
        "Ронни О'Салливан и Марк Уильямс провели успешный сезон 2017/18.\n",
        "Как и Джек Лисовски: рост с отрицательного рейтинга до 13-го места."
      )
    ) +
    theme(title = element_text(size = 13), strip.text = element_text(size = 12))
}

plot_top16_elobeta_history(top16_rating_evolution)
