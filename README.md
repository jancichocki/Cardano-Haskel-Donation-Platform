# Haskel Donation Platform

## Introduction

Welcome to the **Haskel Donation Platform**—a comprehensive solution for managing donations, project verification, impact reporting, and blockchain integration. Built with Haskell, this platform ensures transparency, security, and efficiency in handling charitable contributions while leveraging the Cardano blockchain for immutable audit trails and transaction management.

This project is ideal for organizations looking to streamline their donation processes, ensure accountability, and leverage cutting-edge blockchain technology to enhance donor trust and project credibility.

## Key Features

### 1. **User Registration and Donation Management**
   - **User Registration**: Collect and verify user details through a streamlined registration process.
   - **KYC Verification**: Implement basic Know Your Customer (KYC) protocols to ensure user authenticity.
   - **Donation Allocation**: Allow donors to allocate funds to specific categories such as Education, Healthcare, Environment, Animal Welfare, and Community Development.
   - **Real-time Dashboard**: Automatically update and display donation statistics, including total funds and projects funded.

### 2. **Project Verification and Approval**
   - **Project Verification**: Verify projects before they receive funding to ensure legitimacy.
   - **Approval Workflow**: Approve verified projects, making them eligible for donations.
   - **Audit Trail**: Maintain an immutable record of all project approvals and verifications, ensuring transparency.

### 3. **Impact Measurement and Reporting**
   - **Impact Calculation**: Measure the impact of donations across various projects and categories.
   - **Comprehensive Reporting**: Generate detailed reports on donation distributions, project progress, and overall impact.
   - **Data Analytics**: Use built-in analytics to visualize trends and insights from donation data.

### 4. **Cardano Blockchain Integration**
   - **Blockchain Transactions**: Seamlessly integrate with the Cardano blockchain to submit and manage transactions.
   - **Immutable Audit Trails**: Leverage blockchain immutability to ensure that all transaction records are secure and tamper-proof.
   - **BlockFrost API**: Utilize the BlockFrost API to interact with the Cardano Testnet, facilitating real-time blockchain operations.

## Why Choose the Haskel Donation Platform?

- **Security**: Built with Haskell, a language known for its strong type system and reliability, ensuring that your donation platform is secure and error-resistant.
- **Transparency**: With built-in audit trails and blockchain integration, all transactions and project approvals are transparent and verifiable.
- **Efficiency**: The platform streamlines the donation process, from user registration to project funding and reporting, reducing administrative overhead.
- **Scalability**: Designed to handle multiple projects and large volumes of donations, making it suitable for both small and large organizations.
- **Blockchain-Ready**: Future-proof your operations by integrating with the Cardano blockchain, ensuring your platform is ready for the next generation of financial technology.

## Getting Started

### Prerequisites

Before you begin, ensure you have the following installed on your system:

- [Haskell](https://www.haskell.org/downloads/) - The programming language used to build this platform.
- [Cabal](https://www.haskell.org/cabal/download.html) - A system for building and packaging Haskell libraries and programs.
- A **BlockFrost API Key** for interacting with the Cardano Testnet.

### Installation

1. **Clone the Repository**:
    ```bash
    git clone https://github.com/your-username/haskel-donation-platform.git
    cd haskel-donation-platform
    ```

2. **Install Dependencies**:
    ```bash
    cabal update
    cabal install --lib text aeson time random http-conduit http-client-tls
    ```

3. **Build the Project**:
    ```bash
    cabal build
    ```

### Configuration

Ensure that your BlockFrost API key is correctly set up in the `CardanoTransactions.hs` file:

```haskell
projectId :: String
projectId = "your-blockfrost-api-key"
