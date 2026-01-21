package com.card.management.Services;

import com.card.management.DTOs.TransactionListResponseDto;
import com.card.management.DTOs.TransactionListRequestDto;
import com.card.management.Models.TransactionRecord;
import com.card.management.Repositories.TransactionRecordRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TransactionListService {

  private final TransactionRecordRepository transactionRecordRepository;
  private static final int PAGE_SIZE = 10; // Equivalente a las 10 filas del COBOL
  private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("MM/dd/yy");

  public TransactionListResponseDto getTransactionList(TransactionListRequestDto request) {
    try {
      // Validar si el transaction ID es numérico (como en COBOL)
      if (request.getTransactionId() != null && !request.getTransactionId().trim().isEmpty()) {
        if (!isNumeric(request.getTransactionId())) {
          return createErrorResponse("Tran ID must be Numeric ...");
        }
      }

      return processPageForward(request);

    } catch (Exception e) {
      return createErrorResponse("Unable to lookup transaction...");
    }
  }

  // Equivalente a PROCESS-PAGE-FORWARD
  public TransactionListResponseDto processPageForward(TransactionListRequestDto request) {
    String startId = request.getTransactionId() != null && !request.getTransactionId().trim().isEmpty()
        ? request.getTransactionId()
        : "0";

    Pageable pageable = PageRequest.of(0, PAGE_SIZE + 1); // +1 para verificar si hay siguiente página
    Page<TransactionRecord> transactionPage = transactionRecordRepository.findTransactionsFromId(startId, pageable);

    return buildResponse(transactionPage, request.getPageNumber() != null ? request.getPageNumber() + 1 : 1);
  }

  // Equivalente a PROCESS-PAGE-BACKWARD
  public TransactionListResponseDto processPageBackward(TransactionListRequestDto request) {
    if (request.getPageNumber() == null || request.getPageNumber() <= 1) {
      return createErrorResponse("You are already at the top of the page...");
    }

    String startId = request.getTransactionId() != null ? request.getTransactionId() : "";
    Pageable pageable = PageRequest.of(0, PAGE_SIZE + 1);
    Page<TransactionRecord> transactionPage = transactionRecordRepository.findTransactionsBeforeId(startId, pageable);

    return buildResponse(transactionPage, request.getPageNumber() - 1);
  }

  private TransactionListResponseDto buildResponse(Page<TransactionRecord> transactionPage, Integer pageNumber) {
    List<TransactionRecord> transactions = transactionPage.getContent();

    // Limitar a 10 registros para mostrar
    List<TransactionRecord> displayTransactions = transactions.stream()
        .limit(PAGE_SIZE)
        .collect(Collectors.toList());

    // Verificar si hay siguiente página
    boolean hasNextPage = transactions.size() > PAGE_SIZE;
    boolean hasPreviousPage = pageNumber > 1;

    // Convertir a DTOs
    List<TransactionListResponseDto.TransactionItem> transactionItems = displayTransactions.stream()
        .map(this::convertToTransactionItem)
        .collect(Collectors.toList());

    TransactionListResponseDto response = new TransactionListResponseDto();
    response.setTransactions(transactionItems);
    response.setCurrentPage(pageNumber);
    response.setHasNextPage(hasNextPage);
    response.setHasPreviousPage(hasPreviousPage);

    if (!transactionItems.isEmpty()) {
      response.setFirstTransactionId(transactionItems.get(0).getTransactionId());
      response.setLastTransactionId(transactionItems.get(transactionItems.size() - 1).getTransactionId());
    }

    return response;
  }

  // Método que convierte TransactionRecord a TransactionItem DTO
  private TransactionListResponseDto.TransactionItem convertToTransactionItem(TransactionRecord transaction) {
    TransactionListResponseDto.TransactionItem item = new TransactionListResponseDto.TransactionItem();
    item.setTransactionId(transaction.getTransactionId()); // TRAN_ID
    item.setDescription(transaction.getTransactionDescription()); // TRAN_DESC
    item.setAmount(transaction.getTransactionAmount()); // TRAN_AMT

    // Formatear fecha como en COBOL (MM/DD/YY) usando TRAN_ORIG_TS
    if (transaction.getOriginalTimestamp() != null) {
      item.setDate(transaction.getOriginalTimestamp().format(DATE_FORMATTER));
    } else {
      item.setDate("00/00/00"); // Valor por defecto como en COBOL
    }

    return item;
  }

  // Validar si una cadena es numérica (equivalente a la validación COBOL)
  private boolean isNumeric(String str) {
    try {
      Long.parseLong(str);
      return true;
    } catch (NumberFormatException e) {
      return false;
    }
  }

  // Crear respuesta de error
  private TransactionListResponseDto createErrorResponse(String errorMessage) {
    TransactionListResponseDto response = new TransactionListResponseDto();
    response.setErrorMessage(errorMessage);
    return response;
  }
}
