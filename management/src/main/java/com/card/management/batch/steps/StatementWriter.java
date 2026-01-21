package com.card.management.batch.steps;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.io.IOException;
import java.nio.file.Paths;
import java.nio.file.Path;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.stereotype.Component;
import org.springframework.beans.factory.annotation.Value;

import com.card.management.DTOs.StatementGenerationResultDto;


@Component
public class StatementWriter implements ItemWriter<StatementGenerationResultDto> {

  private static final Logger logger = LoggerFactory.getLogger(StatementWriter.class);

  @Value("${statement.output.text.path:./statements/text/}")
  private String textOutputPath;

  @Value("${statement.output.html.path:./statements/html/}")
  private String htmlOutputPath;

  @Override
  public void write(Chunk<? extends StatementGenerationResultDto> chunk) throws Exception {
    // Crear directorios si no existen
    Files.createDirectories(Paths.get(textOutputPath));
    Files.createDirectories(Paths.get(htmlOutputPath));

    for (StatementGenerationResultDto result : chunk) {
      if (result.isSuccess()) {
        try {
          // Escribir archivo de texto - equivalente a STMTFILE
          String textFileName = String.format("statement_%s.txt", result.getAccountId());
          Path textFilePath = Paths.get(textOutputPath, textFileName);
          Files.write(textFilePath, result.getTextStatement().getBytes(StandardCharsets.UTF_8));

          // Escribir archivo HTML - equivalente a HTMLFILE
          String htmlFileName = String.format("statement_%s.html", result.getAccountId());
          Path htmlFilePath = Paths.get(htmlOutputPath, htmlFileName);
          Files.write(htmlFilePath, result.getHtmlStatement().getBytes(StandardCharsets.UTF_8));

          logger.info("Statement generated successfully for account: {}", result.getAccountId());

        } catch (IOException e) {
          logger.error("Error writing statement files for account: {}", result.getAccountId(), e);
          throw new RuntimeException("Failed to write statement files", e);
        }
      } else {
        logger.error("Failed to generate statement for account: {} - Error: {}",
            result.getAccountId(), result.getErrorMessage());
      }
    }
  }
}
