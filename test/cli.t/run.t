Validate payment details:

  $ payments iban "gb82 west 1234 5698 7654 32"
  ("GB82 WEST 1234 5698 7654 32" (country GB) (sepa true))
  $ payments iban DE89370400440532013001
  (invalid ("IBAN check digits do not match" (raw DE89370400440532013001)))
  [1]
  $ payments address -network tron TR7NHqjeKQxGTCi8q8ZY4pL8otSzgjLj6t
  ((network Tron) (address TR7NHqjeKQxGTCi8q8ZY4pL8otSzgjLj6t))
  $ payments lightning satoshi@example.com
  ((t (Address (user satoshi) (domain example.com)))
   (pay_url (https://example.com/.well-known/lnurlp/satoshi)))
  $ payments schedule -monthly 31 -from 2027-01-15 -count 3
  2027-01-31
  2027-02-28
  2027-03-31

A sample transfer that fails after submission releases its hold:

  $ payments demo -fail
  evt-deposit/deposit
    Debit  settlement:sepa_credit/EUR       100.00 EUR
    Credit customer:demo/eur                100.00 EUR
  -> Submitted
  evt-1/hold
    Debit  customer:demo/eur                25.50 EUR
    Credit customer:demo/eur/pending        25.50 EUR
  -> Rejected
  evt-2/release
    Debit  customer:demo/eur/pending        25.50 EUR
    Credit customer:demo/eur                25.50 EUR
  available 100.00 EUR
  ACCOUNT                     CURRENCY                DEBIT               CREDIT
  customer:demo/eur           EUR                                         100.00
  settlement:sepa_credit/EUR  EUR                    100.00
  TOTAL                       EUR                    100.00               100.00
