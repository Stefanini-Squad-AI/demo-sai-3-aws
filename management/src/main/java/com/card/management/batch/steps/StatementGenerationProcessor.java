package com.card.management.batch.steps;

import java.math.BigDecimal;
import java.util.List;

import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.card.management.DTOs.StatementDataDto;
import com.card.management.DTOs.StatementGenerationResultDto;
import com.card.management.Models.Account;
import com.card.management.Models.CardXrefRecord;
import com.card.management.Models.Customer;
import com.card.management.Models.TransactionReport;
import com.card.management.Repositories.AccountRepository;
import com.card.management.Repositories.CustomerRepository;
import com.card.management.Repositories.TransactionReportRepository;

@Component
public class StatementGenerationProcessor implements ItemProcessor<CardXrefRecord, StatementGenerationResultDto> {
  @Autowired
  private CustomerRepository customerRepository;

  @Autowired
  private AccountRepository accountRepository;

  @Autowired
  private TransactionReportRepository transactionReportRepository;

  @Override
  public StatementGenerationResultDto process(CardXrefRecord cardXref) throws Exception {
    try {
      // Obtener datos del cliente - equivalente a 2000-CUSTFILE-GET
      Customer customer = customerRepository.findById(cardXref.getCustomerId())
          .orElseThrow(() -> new RuntimeException("Customer not found: " + cardXref.getCustomerId()));

      // Obtener datos de la cuenta - equivalente a 3000-ACCTFILE-GET
      Account account = accountRepository.findById(cardXref.getAccountId())
          .orElseThrow(() -> new RuntimeException("Account not found: " + cardXref.getAccountId()));

      // Obtener transacciones para esta tarjeta - equivalente a 4000-TRNXFILE-GET
      List<TransactionReport> transactions = transactionReportRepository
          .findByCardNumberOrderByTransactionId(cardXref.getCardNumber());

      // Calcular total de transacciones - equivalente a WS-TOTAL-AMT
      BigDecimal totalAmount = transactions.stream()
          .map(TransactionReport::getTransactionAmount)
          .reduce(BigDecimal.ZERO, BigDecimal::add);

      // Crear datos del statement
      StatementDataDto statementData = new StatementDataDto();
      statementData.setCardXref(cardXref);
      statementData.setCustomer(customer);
      statementData.setAccount(account);
      statementData.setTransactions(transactions);
      statementData.setTotalAmount(totalAmount);

      // Generar statement en texto - equivalente a 5000-CREATE-STATEMENT
      String textStatement = generateTextStatement(statementData);

      // Generar statement en HTML - equivalente a las funciones HTML
      String htmlStatement = generateHtmlStatement(statementData);

      StatementGenerationResultDto result = new StatementGenerationResultDto();
      result.setAccountId(account.getAccountId());
      result.setTextStatement(textStatement);
      result.setHtmlStatement(htmlStatement);
      result.setSuccess(true);

      return result;

    } catch (Exception e) {
      StatementGenerationResultDto result = new StatementGenerationResultDto();
      result.setAccountId(cardXref.getAccountId());
      result.setSuccess(false);
      result.setErrorMessage(e.getMessage());
      return result;
    }
  }

  // Equivalente a 5000-CREATE-STATEMENT y funciones relacionadas
  private String generateTextStatement(StatementDataDto data) {
    StringBuilder statement = new StringBuilder();

    // ST-LINE0 - Header
    statement.append("*".repeat(31))
        .append("START OF STATEMENT")
        .append("*".repeat(31))
        .append("\n");

    // ST-LINE1 - Customer name
    String customerName = String.format("%s %s %s",
        data.getCustomer().getFirstName().trim(),
        data.getCustomer().getMiddleName().trim(),
        data.getCustomer().getLastName().trim()).trim();
    statement.append(String.format("%-75s%5s\n", customerName, ""));

    // ST-LINE2, ST-LINE3, ST-LINE4 - Address
    statement.append(String.format("%-50s%30s\n", data.getCustomer().getAddressLine1(), ""));
    statement.append(String.format("%-50s%30s\n", data.getCustomer().getAddressLine2(), ""));

    String addressLine3 = String.format("%s %s %s %s",
        data.getCustomer().getAddressLine3().trim(),
        data.getCustomer().getStateCode().trim(),
        data.getCustomer().getCountryCode().trim(),
        data.getCustomer().getZipCode().trim()).trim();
    statement.append(String.format("%-80s\n", addressLine3));

    // ST-LINE5 - Separator
    statement.append("-".repeat(80)).append("\n");

    // ST-LINE6 - Basic Details header
    statement.append(String.format("%33s%-14s%33s\n", "", "Basic Details", ""));

    // ST-LINE7 - Account ID
    statement.append(String.format("%-20s%-20s%40s\n", "Account ID         :",
        data.getAccount().getAccountId(), ""));

    // ST-LINE8 - Current Balance
    statement.append(String.format("%-20s%11.2f%7s%40s\n", "Current Balance    :",
        data.getAccount().getCurrentBalance(), "", ""));

    // ST-LINE9 - FICO Score
    statement.append(String.format("%-20s%-20s%40s\n", "FICO Score         :",
        data.getCustomer().getFicoScore(), ""));

    // ST-LINE10, ST-LINE11, ST-LINE12 - Transaction header
    statement.append("-".repeat(80)).append("\n");
    statement.append(String.format("%30s%-20s%30s\n", "", "TRANSACTION SUMMARY ", ""));
    statement.append("-".repeat(80)).append("\n");
    statement.append(String.format("%-16s%-51s%13s\n", "Tran ID         ",
        "Tran Details    ", "  Tran Amount"));
    statement.append("-".repeat(80)).append("\n");

    // ST-LINE14 - Transaction details (equivalente a 6000-WRITE-TRANS)
    for (TransactionReport transaction : data.getTransactions()) {
      statement.append(String.format("%-16s %-49s$%11.2f\n",
          transaction.getTransactionId(),
          transaction.getTransactionDescription(),
          transaction.getTransactionAmount()));
    }

    // ST-LINE14A - Total
    statement.append("-".repeat(80)).append("\n");
    statement.append(String.format("%-10s%56s$%11.2f\n", "Total EXP:", "", data.getTotalAmount()));

    // ST-LINE15 - Footer
    statement.append("*".repeat(32))
        .append("END OF STATEMENT")
        .append("*".repeat(32))
        .append("\n");

    return statement.toString();
  }

  // Equivalente a las funciones HTML del COBOL
  private String generateHtmlStatement(StatementDataDto data) {
    StringBuilder html = new StringBuilder();

    // HTML Header - equivalente a 5100-WRITE-HTML-HEADER
    html.append("<!DOCTYPE html>\n")
        .append("<html lang=\"en\">\n")
        .append("<head>\n")
        .append("<meta charset=\"utf-8\">\n")
        .append("<title>HTML Table Layout</title>\n")
        .append("</head>\n")
        .append("<body style=\"margin:0px;\">\n")
        .append("<table align=\"center\" frame=\"box\" style=\"width:70%; font:12px Segoe UI,sans-serif;\">\n")
        .append("<tr>\n")
        .append("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">\n")
        .append("<h3>Statement for Account Number: ").append(data.getAccount().getAccountId()).append("</h3>\n")
        .append("</td>\n")
        .append("</tr>\n")
        .append("<tr>\n")
        .append("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#FFAF33;\">\n")
        .append("<p style=\"font-size:16px\">Bank of XYZ</p>\n")
        .append("<p>410 Terry Ave N</p>\n")
        .append("<p>Seattle WA 99999</p>\n")
        .append("</td>\n")
        .append("</tr>\n")
        .append("<tr>\n")
        .append("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#f2f2f2;\">\n");

    // Customer information - equivalente a 5200-WRITE-HTML-NMADBS
    String customerName = String.format("%s %s %s",
        data.getCustomer().getFirstName().trim(),
        data.getCustomer().getMiddleName().trim(),
        data.getCustomer().getLastName().trim()).trim();

    html.append("<p style=\"font-size:16px\">").append(customerName).append("</p>\n")
        .append("<p>").append(data.getCustomer().getAddressLine1().trim()).append("</p>\n")
        .append("<p>").append(data.getCustomer().getAddressLine2().trim()).append("</p>\n");

    String addressLine3 = String.format("%s %s %s %s",
        data.getCustomer().getAddressLine3().trim(),
        data.getCustomer().getStateCode().trim(),
        data.getCustomer().getCountryCode().trim(),
        data.getCustomer().getZipCode().trim()).trim();
    html.append("<p>").append(addressLine3).append("</p>\n")
        .append("</td>\n")
        .append("</tr>\n");

    // Basic Details section
    html.append("<tr>\n")
        .append("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#33FFD1; text-align:center;\">\n")
        .append("<p style=\"font-size:16px\">Basic Details</p>\n")
        .append("</td>\n")
        .append("</tr>\n")
        .append("<tr>\n")
        .append("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#f2f2f2;\">\n")
        .append("<p>Account ID         : ").append(data.getAccount().getAccountId()).append("</p>\n")
        .append("<p>Current Balance    : ").append(data.getAccount().getCurrentBalance()).append("</p>\n")
        .append("<p>FICO Score         : ").append(data.getCustomer().getFicoScore()).append("</p>\n")
        .append("</td>\n")
        .append("</tr>\n");

    // Transaction Summary header
    html.append("<tr>\n")
        .append("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#33FFD1; text-align:center;\">\n")
        .append("<p style=\"font-size:16px\">Transaction Summary</p>\n")
        .append("</td>\n")
        .append("</tr>\n")
        .append("<tr>\n")
        .append("<td style=\"width:25%; padding:0px 5px; background-color:#33FF5E; text-align:left;\">\n")
        .append("<p style=\"font-size:16px\">Tran ID</p>\n")
        .append("</td>\n")
        .append("<td style=\"width:55%; padding:0px 5px; background-color:#33FF5E; text-align:left;\">\n")
        .append("<p style=\"font-size:16px\">Tran Details</p>\n")
        .append("</td>\n")
        .append("<td style=\"width:20%; padding:0px 5px; background-color:#33FF5E; text-align:right;\">\n")
        .append("<p style=\"font-size:16px\">Amount</p>\n")
        .append("</td>\n")
        .append("</tr>\n");

    // Transaction details - equivalente a 6000-WRITE-TRANS (parte HTML)
    for (TransactionReport transaction : data.getTransactions()) {
      html.append("<tr>\n")
          .append("<td style=\"width:25%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">\n")
          .append("<p>").append(transaction.getTransactionId()).append("</p>\n")
          .append("</td>\n")
          .append("<td style=\"width:55%; padding:0px 5px; background-color:#f2f2f2; text-align:left;\">\n")
          .append("<p>").append(transaction.getTransactionDescription()).append("</p>\n")
          .append("</td>\n")
          .append("<td style=\"width:20%; padding:0px 5px; background-color:#f2f2f2; text-align:right;\">\n")
          .append("<p>").append(transaction.getTransactionAmount()).append("</p>\n")
          .append("</td>\n")
          .append("</tr>\n");
    }

    // HTML Footer
    html.append("<tr>\n")
        .append("<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">\n")
        .append("<h3>End of Statement</h3>\n")
        .append("</td>\n")
        .append("</tr>\n")
        .append("</table>\n")
        .append("</body>\n")
        .append("</html>\n");

    return html.toString();
  }
}
