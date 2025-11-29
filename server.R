library(shiny)
library(quantmod)
library(ggplot2)

server<- function(input, output, session) {
  wynikiAnalizy <- eventReactive(input$analizuj, {
    alpha_uzytkownika <- as.numeric(isolate(input$poziom_istotnosci))
    ticker_s <- isolate(input$ticker_spolki)
    ticker_r <- isolate(input$ticker_rynku)
    daty <- isolate(input$daty)
    withProgress(message = 'Przetwarzanie danych...', value = 0, {
      
      #Pobranie Danych
      incProgress(0.1, detail = "Pobieranie danych...")
      
      dane_spolki <- try(getSymbols(ticker_s, src = "yahoo", from = daty[1], to = daty[2], auto.assign = FALSE), silent = TRUE)
      dane_rynku <- try(getSymbols(ticker_r, src = "yahoo", from = daty[1], to = daty[2], auto.assign = FALSE), silent = TRUE)
      
      #OBsluga bledow pobierania
      if (inherits(dane_spolki, "try-error") || inherits(dane_rynku, "try-error")) {
        stop("Błąd pobierania danych. Sprawdź ticker lub zakres dat.")
      }
      incProgress(0.2, detail = "Obliczanie stóp zwrotu.
                  ..")
      zwrot_spolki <- dailyReturn(Ad(dane_spolki))
      zwrot_rynku <- dailyReturn(Ad(dane_rynku))
      
      dane <- merge(zwrot_spolki, zwrot_rynku)
      colnames(dane) <- c("y", "x") # y = spółka, x = rynek
      dane <- na.omit(dane)
      
      if (nrow(dane) < 30) {
        stop("Za mało danych w wybranym okresie do przeprowadzenia analizy (wymagane min. 30 obserwacji).")
      }
      
      dane$x_kwadrat <- dane$x^2
      dane_df <- as.data.frame(dane)
      
      incProgress(0.3, detail = "Estymacja modeli...")
      model_A <- lm(y ~ x, data = dane_df)
      model_B <- lm(y ~ x + x_kwadrat, data = dane_df)
  
      porownanie_aic <- AIC(model_A, model_B)
      
      if (porownanie_aic$AIC[1] <= porownanie_aic$AIC[2]) {
        lepszy <- model_A
        nazwa_modelu <- "Model A (Liniowy)"
      } else {
        lepszy <- model_B
        nazwa_modelu <- "Model B (Kwadratowy)"
      }
      
      sum_modelu <- summary(lepszy)
      coef_table <- sum_modelu$coefficients
      
      interpret_html_list <- list()
      
      alpha_est <- coef_table["(Intercept)", "Estimate"]
      alpha_pval <- coef_table["(Intercept)", "Pr(>|t|)"]
      
      if (alpha_pval < alpha_uzytkownika) {
        tekst_alfy <- paste0("Parametr <b>Alfa (Intercept)</b> jest <strong>istotny statystycznie</strong> (p-value = ", 
                             round(alpha_pval, 4), "). Jego wartość to ", round(alpha_est, 5), ". ",
                             "Oznacza to, że spółka generowała ", 
                             ifelse(alpha_est > 0, "ponadprzeciętne", "poniżejprzeciętne"),
                             " stopy zwrotu, nawet po uwzględnieniu jej ryzyka rynkowego.")
      } else {
        tekst_alfy <- paste0("Parametr <b>Alfa (Intercept)</b> jest <strong>nieistotny statystycznie</strong> (p-value = ", 
                             round(alpha_pval, 4), "). ",
                             "Oznacza to, że nie ma statystycznych podstaw by twierdzić, że spółka generowała stopy zwrotu inne niż wynikałoby to z jej ryzyka rynkowego (Alfa jest statystycznie równa zeru).")
      }
      interpret_html_list[[1]] <- tags$p(HTML(tekst_alfy))
      
      beta_est <- coef_table["x", "Estimate"]
      beta_pval <- coef_table["x", "Pr(>|t|)"]
      
      if (beta_pval < alpha_uzytkownika) {
        tekst_bety <- paste0("Parametr <b>Beta (x)</b> jest <strong>istotny statystycznie</strong> (p-value < 0.001). Jego oszacowana wartość to <b>", round(beta_est, 3), "</b>. ",
                             "Oznacza to, że spółka jest ",
                             ifelse(beta_est > 1, "<b>agresywna</b> (wrażliwsza niż rynek).", 
                                    ifelse(beta_est < 1 & beta_est > 0, "<b>defensywna</b> (mniej wrażliwa niż rynek).", "neutralna lub negatywnie skorelowana.")))
      } else {
        tekst_bety <- paste0("Parametr <b>Beta (x)</b> jest <strong>nieistotny statystycznie</strong> (p-value = ", 
                             round(beta_pval, 4), "). Oznacza to, że nie znaleziono statystycznie istotnej liniowej zależności między rynkiem a spółką.")
      }
      interpret_html_list[[2]] <- tags$p(HTML(tekst_bety))
      
      if (nazwa_modelu == "Model B (Kwadratowy)") {
        beta2_est <- coef_table["x_kwadrat", "Estimate"]
        beta2_pval <- coef_table["x_kwadrat", "Pr(>|t|)"]
        
        if (beta2_pval < alpha_uzytkownika) {
          tekst_bety2 <- paste0("Parametr <b>Beta 2 (x_kwadrat)</b> jest <strong>istotny statystycznie</strong> (p-value = ", 
                                round(beta2_pval, 4), "). ",
                                "Oznacza to, że zależność między spółką a rynkiem <b>nie jest liniowa</b>. ",
                                ifelse(beta2_est > 0, "Wartość dodatnia (<b>wypukłość</b>) sugeruje, że spółka reaguje silniej na ekstremalne ruchy rynku (tzw. 'market timing').",
                                       "Wartość ujemna (<b>wklęsłość</b>) sugeruje, że spółka stabilizuje się podczas ekstremalnych ruchów rynku."))
        } else {
          tekst_bety2 <- paste0("Parametr <b>Beta 2 (x_kwadrat)</b> jest <strong>nieistotny statystycznie</strong> (p-value = ", 
                                round(beta2_pval, 4), "). ",
                                "Mimo że model kwadratowy wygrał wg AIC, sam człon nieliniowy nie okazał się istotny (na wybranym poziomie α).")
        }
        interpret_html_list[[3]] <- tags$p(HTML(tekst_bety2))
      }
      
      r_sq <- sum_modelu$adj.r.squared
      tekst_rsq <- paste0("Model wyjaśnia <b>", round(r_sq * 100, 2), "%</b> zmienności (ryzyka) spółki. ",
                          "Jest to wartość <b>Skorygowanego R-kwadrat</b>.")
      interpret_html_list[[4]] <- tags$p(HTML(tekst_rsq))
      
      final_html_output <- do.call(tags$div, interpret_html_list)
      
      incProgress(0.5, detail = "Generowanie wykresu...")
      
      dane_df$pred_A <- predict(model_A, newdata = dane_df)
      dane_df$pred_B <- predict(model_B, newdata = dane_df)
      
      wykres_dopasowania <- ggplot(dane_df, aes(x = x, y = y)) +
        
        geom_point(alpha = 0.3, color = "grey50") + 
        
        geom_line(aes(y = pred_A, color = "Model A (Liniowy)"), size = 1) +
        
        geom_line(aes(y = pred_B, color = "Model B (Kwadratowy)"), size = 1, linetype = "dashed") +
        
        scale_color_manual(
          name = "Porównywane Modele", # Tytuł legendy
          values = c("Model A (Liniowy)" = "blue", "Model B (Kwadratowy)" = "red")
        ) +
        
        labs(
          title = paste("Porównanie graficzne dopasowania modeli dla", ticker_s),
          subtitle = paste("Zwycięzca AIC:", nazwa_modelu, "| Rynek:", ticker_r),
          x = "Dzienna stopa zwrotu rynku (x)",
          y = "Dzienna stopa zwrotu spółki (y)"
        ) +
        
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom") 
      
      incProgress(0.7, detail = "Testowanie reszt...")
      reszty <- residuals(lepszy)
      
      if (length(reszty) > 5000) {
        reszty_test <- sample(reszty, 5000)
        info_sw <- "Testowano na losowej próbce 5000 reszt."
      } else {
        reszty_test <- reszty
        info_sw <- "Testowano na pełnym zbiorze reszt."
      }
      test_sw <- shapiro.test(reszty_test)
      test_sw$info <- info_sw
      
      incProgress(1.0, detail = "Zakończono.")
      
      list(
        porownanie_aic = porownanie_aic,
        wniosek_aic = paste("Wybrano:", nazwa_modelu, "(ponieważ ma niższe AIC)."),
        wykres_dopasowania = wykres_dopasowania,
        podsumowanie_modelu = summary(lepszy),
        test_normalnosci = test_sw,
        reszty = reszty
      )
    }) # Koniec withProgress
    list(
      porownanie_aic = porownanie_aic,
      wniosek_aic = paste("Wybrano:", nazwa_modelu, "(ponieważ ma niższe AIC)."),
      wykres_dopasowania = wykres_dopasowania,
      podsumowanie_modelu = sum_modelu, 
      test_normalnosci = test_sw,
      reszty = reszty,
      interpretacja_html = final_html_output 
    )
    
  }) #Koniec eventReactive
  
  output$aic_output <- renderPrint({
    wynikiAnalizy()$porownanie_aic
  })
  output$wniosek_aic <- renderText({
    wynikiAnalizy()$wniosek_aic
  })
  output$wniosek_aic <- renderText({ wynikiAnalizy()$wniosek_aic })
  
  output$wykres_dopasowania <- renderPlot({
    wynikiAnalizy()$wykres_dopasowania
  })
  output$interpretacja_parametrow <- renderUI({
    wynikiAnalizy()$interpretacja_html
  })
  output$podsumowanie_modelu <- renderPrint({
    wynikiAnalizy()$podsumowanie_modelu
  })
  output$test_normalnosci <- renderPrint({
    test <- wynikiAnalizy()$test_normalnosci
    cat(paste(test$info, "\n\n")) # Wyświetlamy nasze dodatkowe info
    print(test)
  })
  output$wykresy_diagnostyczne <- renderPlot({
    reszty <- wynikiAnalizy()$reszty
    
    #2 wykresy obok siebie
    par(mfrow = c(1, 2), mar = c(4, 4, 2, 1)) 
    
    hist(reszty, breaks = 100, main = "Histogram reszt", col = "gray", xlab = "Reszty")
    
    qqnorm(reszty, main = "Wykres Q-Q reszt")
    qqline(reszty, col = "red", lwd = 2)
    
    par(mfrow = c(1, 1)) 
  })
}
 
   
