package com.card.management.batch.steps;

import com.card.management.DTOs.AuthCleanupResultDto;
import com.card.management.Models.PendingAuthDetail;
import com.card.management.Models.PendingAuthSummary;
import com.card.management.Repositories.PendingAuthDetailRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.List;

@Component
public class AuthCleanupProcessor implements ItemProcessor<PendingAuthSummary, AuthCleanupResultDto> {

  private static final Logger logger = LoggerFactory.getLogger(AuthCleanupProcessor.class);

  @Autowired
  private PendingAuthDetailRepository detailRepository;

  // Equivalente a WS-EXPIRY-DAYS en COBOL, configurable via job parameters
  @Value("${batch.auth.cleanup.expiryDays:5}")
  private int expiryDays;

  @Value("${batch.auth.cleanup.debugFlag:N}")
  private String debugFlag;

  @Override
  public AuthCleanupResultDto process(PendingAuthSummary summary) throws Exception {

    if ("Y".equals(debugFlag)) {
      logger.debug("Processing auth summary for account: {}", summary.getAccountId());
    }

    AuthCleanupResultDto result = new AuthCleanupResultDto(summary.getAccountId());

    // Calcular fecha de expiración - equivalente a CURRENT-YYDDD - WS-EXPIRY-DAYS
    LocalDate expiryDate = LocalDate.now().minusDays(expiryDays);

    // Buscar detalles expirados para esta cuenta
    List<PendingAuthDetail> expiredDetails = detailRepository
        .findExpiredDetailsByAccountId(summary.getAccountId(), expiryDate);

    // Procesar cada detalle expirado - equivalente al loop 3000-FIND-NEXT-AUTH-DTL
    for (PendingAuthDetail detail : expiredDetails) {

      // Equivalente a 4000-CHECK-IF-EXPIRED y ajuste de contadores
      if ("00".equals(detail.getAuthResponseCode())) {
        // Autorización aprobada
        result.setApprovedCountAdjusted(result.getApprovedCountAdjusted() + 1);
        result.setApprovedAmountAdjusted(
            result.getApprovedAmountAdjusted().add(detail.getApprovedAmount()));

        // Actualizar contadores en el summary
        summary.setApprovedAuthCount(summary.getApprovedAuthCount() - 1);
        summary.setApprovedAuthAmount(
            summary.getApprovedAuthAmount().subtract(detail.getApprovedAmount()));
      } else {
        // Autorización declinada
        result.setDeclinedCountAdjusted(result.getDeclinedCountAdjusted() + 1);
        result.setDeclinedAmountAdjusted(
            result.getDeclinedAmountAdjusted().add(detail.getTransactionAmount()));

        // Actualizar contadores en el summary
        summary.setDeclinedAuthCount(summary.getDeclinedAuthCount() - 1);
        summary.setDeclinedAuthAmount(
            summary.getDeclinedAuthAmount().subtract(detail.getTransactionAmount()));
      }

      result.setDetailsDeleted(result.getDetailsDeleted() + 1);
    }

    // Verificar si el summary debe eliminarse - equivalente a la condición en
    // MAIN-PARA
    if (summary.getApprovedAuthCount() <= 0 && summary.getDeclinedAuthCount() <= 0) {
      result.setSummaryDeleted(true);
    }

    if ("Y".equals(debugFlag)) {
      logger.debug("Account {} - Details deleted: {}, Summary deleted: {}",
          summary.getAccountId(), result.getDetailsDeleted(), result.isSummaryDeleted());
    }

    return result;
  }
}
