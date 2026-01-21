package com.card.management.batch.steps;

import com.card.management.DTOs.AuthCleanupResultDto;
import com.card.management.Repositories.PendingAuthDetailRepository;
import com.card.management.Repositories.PendingAuthSummaryRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import java.time.LocalDate;

@Component
public class AuthCleanupWriter implements ItemWriter<AuthCleanupResultDto> {

  private static final Logger logger = LoggerFactory.getLogger(AuthCleanupWriter.class);

  @Autowired
  private PendingAuthDetailRepository detailRepository;

  @Autowired
  private PendingAuthSummaryRepository summaryRepository;

  @Value("${batch.auth.cleanup.expiryDays:5}")
  private int expiryDays;

  @Value("${batch.auth.cleanup.debugFlag:N}")
  private String debugFlag;

  // Contadores equivalentes a las variables WS en COBOL
  private int totalSummaryDeleted = 0;
  private int totalDetailsDeleted = 0;

  @Override
  public void write(@NonNull Chunk<? extends AuthCleanupResultDto> chunk) throws Exception {

    LocalDate expiryDate = LocalDate.now().minusDays(expiryDays);

    for (AuthCleanupResultDto result : chunk) {

      // Eliminar detalles expirados - equivalente a 5000-DELETE-AUTH-DTL
      if (result.getDetailsDeleted() > 0) {
        detailRepository.deleteExpiredDetailsByAccountId(
            result.getAccountId(), expiryDate);
        totalDetailsDeleted += result.getDetailsDeleted();

        if ("Y".equals(debugFlag)) {
          logger.debug("Deleted {} expired details for account {}",
              result.getDetailsDeleted(), result.getAccountId());
        }
      }

      // Eliminar summary si no tiene autorizaciones - equivalente a
      // 6000-DELETE-AUTH-SUMMARY
      if (result.isSummaryDeleted()) {
        summaryRepository.deleteById(result.getAccountId());
        totalSummaryDeleted++;

        if ("Y".equals(debugFlag)) {
          logger.debug("Deleted summary for account {}", result.getAccountId());
        }
      }
    }
  }

  // Método para obtener estadísticas finales
  public int getTotalSummaryDeleted() {
    return totalSummaryDeleted;
  }

  public int getTotalDetailsDeleted() {
    return totalDetailsDeleted;
  }
}
